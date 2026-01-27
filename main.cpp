#include "clang-c/CXString.h"
#include <cassert>
#include <cctype>
#include <clang-c/Index.h>
#include <array>
#include <cstddef>
#include <iostream>
#include <unordered_map>
#include <variant>
#include <vector>
#include <format>
#include <optional>

template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };

// Some compilers might require this explicit deduction guide
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

struct UnionDecl;
struct StructDecl;
struct EnumDecl;
struct TypeDefDecl;

// pseja was here
// using Type = std::variant<std::unique_ptr<UnionDecl>, std::unique_ptr<StructDecl>, std::unique_ptr<EnumDecl>, std::unique_ptr<TypeDefDecl>, std::string>;

struct NameTypePair final
{
    std::string name;
    std::string t;
    std::optional<std::string> typeNoPointer = std::nullopt;
};

using FunctionParam = NameTypePair;
using Field = NameTypePair;

struct FunctionDecl final
{
    std::string funcName;
    std::string funcType;
    std::vector<FunctionParam> params;
    std::optional<std::string> returnTypeNoPointer = std::nullopt;
};

struct UnionDecl final
{
    std::string name;   
    std::vector<Field> fields;
};

struct StructDecl final
{
    std::string name;
    std::vector<Field> fields;
};

struct EnumDecl final
{
    std::string name;
    std::vector<std::string> decls;
};

struct TypeDefDecl final
{
    std::string typeDef;
};

using ASTNode = std::variant<FunctionDecl, UnionDecl, StructDecl, EnumDecl, TypeDefDecl>;

namespace StringUtil
{
void ToLowerCase(std::string& str)
{
    for(char& c : str)
    {
        c = std::tolower(c);
    }
}

void RemoveStruct(std::string& str)
{
    static constexpr std::string_view _struct = "struct";
    std::string::size_type positionToErase = str.find(_struct);
    if(positionToErase == std::string::npos) return;
    str.erase(positionToErase, _struct.length());
}
} // namespace StringUtil

static std::string GetNodeName(const ASTNode& node)
{
    return std::visit(overloaded{
        [](const FunctionDecl& fd) -> std::string 
        {
            return fd.funcName;
        },
        [](const StructDecl& sd) -> std::string
        {
            return sd.name;
        },
        [](const UnionDecl& ud) -> std::string
        {
            return ud.name;
        },
        [](const TypeDefDecl& tdd) -> std::string
        {
            return tdd.typeDef;
        },
        [](const EnumDecl& ed) -> std::string
        {
            return ed.name;
        }
    }, node);
}

struct AST final
{
    std::vector<ASTNode> nodes;
    std::unordered_map<std::string, ASTNode*> nodeNameToNode;
};

static void PushNodeToAST(AST& ast, ASTNode&& node)
{
    std::string nodeName = GetNodeName(node);
    ast.nodes.emplace_back(std::move(node));
    ast.nodeNameToNode.emplace(nodeName, &ast.nodes.back());
}

static std::optional<std::string> GetTypeHandle(const AST& ast, std::string_view type)
{
    if(type == "const char *")
    {
        return std::optional<std::string>("String_val");
    } else if (type == "unsigned char*")
    {
        return std::optional<std::string>("Bytes_val");
    } else if (type == "int")
    {
        return std::optional<std::string>("Int_val");
    } else if (type == "unsigned int")
    {
        return std::optional<std::string>("Int32_val");
    } else if (type == "bool")
    {
        return std::optional<std::string>("Bool_val");
    } else if (type == "float" || type == "double")
    {
        return std::optional<std::string>("Double_val");
    }

    // TODO: This is disgusting - whyyyy can't I use view???
    std::string copy(type);
    if(ast.nodeNameToNode.contains(copy))
    {
        return std::optional<std::string>(std::format("{}_val", copy)); 
    }

    return std::nullopt;
}


// Move if checks to for loop like in below
static std::optional<std::string> TypeToOcamlTypeHandle(const AST& ast, std::string_view type)
{
    if(type == "const char *")
    {
        return std::optional<std::string>("Val_string");
    } else if (type == "unsigned char*")
    {
        return std::optional<std::string>("Val_bytes");
    } else if (type == "int")
    {
        return std::optional<std::string>("Val_int");
    } else if (type == "unsigned int")
    {
        return std::optional<std::string>("Int32_val");
    } else if (type == "bool")
    {
        return std::optional<std::string>("Val_bool");
    } else if (type == "float" || type == "double")
    {
        return std::optional<std::string>("Val_double");
    }

    // TODO: This is disgusting - whyyyy can't I use view???
    std::string copy(type);
    if(ast.nodeNameToNode.contains(copy))
    {
        return std::optional<std::string>(std::format("{}_val", copy)); 
    }

    return std::nullopt;
}

std::string CTypeToOCamlType(const AST& , std::string_view type)
{
    using namespace std::literals;
    static constexpr auto ts = 
    {
        std::tuple("const char*"sv,"string"sv),
        std::tuple("int"sv,"int"sv),
        std::tuple("void"sv,"unit"sv),
        std::tuple("bool"sv,"bool"sv),
        std::tuple("int32_t"sv,"int32"sv),
        std::tuple("int64_t"sv,"int64"sv),
        std::tuple("double"sv,"float"sv),
        std::tuple("float"sv,"float"sv),
        std::tuple("char*"sv,"bytes"sv),

        std::tuple("wchar_t*"sv,"string"sv),
        std::tuple("const wchar_t*"sv,"string"sv),
        std::tuple("unsigned int"sv,"int32"sv),
        std::tuple("size_t"sv,"nativeint"sv),
        std::tuple("ptrdiff_t"sv,"nativeint"sv),
        std::tuple("long"sv,"int64"sv),
        std::tuple("unsigned long"sv,"int64"sv),
    };

    for(auto [from,to] : ts)
    {
        if(from == type) return std::string(to);
    }

    // TODO: handle structs and other types from ast
    return "";
}

struct StringBuilder final
{
    explicit StringBuilder(size_t reserveSize)
    {
        m_finalString.reserve(reserveSize);
    }
    
    StringBuilder& operator+=(const std::string& str)
    {
        std::string whiteSpace(m_offset, ' ');
        m_finalString += std::format("{}{}",whiteSpace, str);
        return *this;
    }

    StringBuilder& operator+=(std::string&& str)
    {
        std::string whiteSpace(m_offset, ' ');
        m_finalString += std::format("{}{}",whiteSpace, std::move(str));
        return *this;
    }

    // << No offset
    StringBuilder& operator<<(const std::string& str)
    {
        m_finalString += str;
        return *this;
    }

    StringBuilder& operator<<(std::string&& str)
    {
        m_finalString += std::move(str);
        return *this;
    }

    void IncreaseOffset() noexcept
    {
        m_offset += 4;
    }

    void DecreaseOffet() noexcept
    {
        m_offset -= 4;
    }

    void Assign(std::string&& str)
    {
        m_finalString = std::move(str);
    }

    std::string GetFinalString()
    {
        return m_finalString;
    }

private: 
    std::string m_finalString;
    int m_offset = 0;
};

struct StringOffsetHelper final
{
    explicit StringOffsetHelper(StringBuilder& builder) : m_builder(builder)
    {
        m_builder.IncreaseOffset();
    }

    ~StringOffsetHelper()
    {
        m_builder.DecreaseOffet();
    }

    StringOffsetHelper(StringBuilder&&) = delete;
    StringOffsetHelper(const StringBuilder&) = delete;
    StringOffsetHelper& operator=(StringBuilder&&) = delete;
    StringOffsetHelper& operator=(const StringBuilder&) = delete;
private:
    StringBuilder& m_builder;
};


static std::string SerializeFunctionDeclToOcamlStub(const AST& ast, const FunctionDecl& fd)
{
    StringBuilder ocamlStubBuilder{1024};
    ocamlStubBuilder += std::format("external c_{} : ", fd.funcName);


    for(const NameTypePair& param : fd.params)
    {
        std::string convertedParamType = CTypeToOCamlType(ast, param.typeNoPointer.value_or(param.t));
        ocamlStubBuilder += std::format("{}:{} -> ", param.name, convertedParamType);
    }

    std::string convertedReturnType = CTypeToOCamlType(ast, fd.returnTypeNoPointer.value_or(fd.funcType));
    ocamlStubBuilder += std::format("{}", convertedReturnType);
    ocamlStubBuilder += std::format(" = \"caml_{}\"", fd.funcName);

    return ocamlStubBuilder.GetFinalString();
}

static std::string SerializeNodeToOCamlStub(const AST& ast, const ASTNode& node)
{
    std::string ocamlStubBuilder;

    std::visit(overloaded{
        [&](const FunctionDecl& fd) -> void 
        {
            ocamlStubBuilder = SerializeFunctionDeclToOcamlStub(ast, fd);
        },
        [&](const StructDecl&) -> void
        {
        },
        [&](const UnionDecl& ) -> void
        {
        },
        [&](const TypeDefDecl& ) -> void
        {
        },
        [&](const EnumDecl&) -> void
        {
        }
    }, node);

    return ocamlStubBuilder;
}

static std::string SerializeFunctionDeclToCStub(const AST& ast, const FunctionDecl& fd)
{
    StringBuilder cStubBuilder{1024};
    size_t paramsCount = fd.params.size();
    if(paramsCount <= 5)
    {
        // Build function name and then args
        cStubBuilder += std::format("CAMLprim value caml_{}(", fd.funcName);
        for(size_t i = 0; i < paramsCount; ++i)
        {
            const auto& param = fd.params.at(i);
            if(i == paramsCount - 1)
                cStubBuilder << std::format("value {}", param.name);
            else
                cStubBuilder << std::format("value {}, ", param.name);
        }
        cStubBuilder += ")\n";
        cStubBuilder += "{\n";

        {
            StringOffsetHelper helper{cStubBuilder};
            cStubBuilder += std::format("CAMLparam{}(", paramsCount);
            // TODO: seems like argument format is kinda repetetive
            for(size_t i = 0; i < paramsCount; ++i)
            {
                const auto& param = fd.params.at(i);
                if(i == paramsCount - 1)
                    cStubBuilder << std::format("{}", param.name);
                else
                    cStubBuilder << std::format("{}, ", param.name);
            }
            cStubBuilder << std::format(");\n");


            StringBuilder argumentBuilder {256};
            const bool isFuncVoid = fd.funcType == "void";
            if(isFuncVoid)
            {
                argumentBuilder += std::format("{}(", fd.funcName);
            }
            else
            {
                cStubBuilder += "CAMLlocal1(returnResult);\n";
                argumentBuilder += std::format("{} result = {}(", fd.funcType , fd.funcName);
            }


            // Get values to C Values from ocaml and call C function 
            for(size_t i = 0; i < paramsCount; ++i)
            {
                const auto& param = fd.params.at(i);

                std::optional<std::string> maybeHandle = GetTypeHandle(ast, param.typeNoPointer.value_or(param.t));
                if(maybeHandle.has_value())
                    cStubBuilder += std::format("{} native_{} = {}({});\n", param.t, param.name, *maybeHandle, param.name);
                else
                    cStubBuilder += std::format("{} native_{} = {}_val({});\n", param.t, param.name, *param.typeNoPointer, param.name);
                argumentBuilder += i != paramsCount - 1 ? std::format("native_{}, ", param.name) : std::format("native_{}", param.name);
            }
            argumentBuilder += ");\n";

            cStubBuilder += argumentBuilder.GetFinalString();

            if(isFuncVoid)
                cStubBuilder += "CAMLreturn(Val_unit);\n";
            else 
            {
                // cStubBuilder += std::format("CAMLreturnT({}, result);\n", fd.funcType);
                std::optional<std::string> maybeOcamlType = TypeToOcamlTypeHandle(ast, fd.funcType);
                if(maybeOcamlType.has_value())
                {
                    cStubBuilder += std::format("returnResult = {}(result);\n", *maybeOcamlType);
                    cStubBuilder += "CAMLreturn(returnResult);\n";
                } else
                {
                    const std::string structName = fd.returnTypeNoPointer.value_or(fd.funcType);
                    std::string structNameLower = structName;
                    StringUtil::ToLowerCase(structNameLower);
                    cStubBuilder += std::format("returnResult = caml_alloc_custom(&{}_ops, sizeof({} *), 0, 1);\n", structNameLower, structName);
                    cStubBuilder += std::format("{}_val(returnResult) = result;\n", structName);
                    cStubBuilder += "CAMLreturn(returnResult);\n";
                }
            }
        }
        
        cStubBuilder += "}\n\n";
    } else {
        assert(false && "TO be added");
    }

    return cStubBuilder.GetFinalString();
}


static std::string SerializeStructDeclToCStub(const AST& ast, const StructDecl& sd)
{
    StringBuilder cStubBuilder{1024};
    const std::string nameLowerCase = std::invoke([copy = std::string(sd.name)]() mutable -> std::string {
        StringUtil::ToLowerCase(copy);
        return copy;
    });
    // Create macro to get value from ocaml
    cStubBuilder += std::format("#define {}_val(v) (*(struct {} **)Data_custom_val(v))\n\n", sd.name, sd.name);

    // Function for defining custom operation for ocaml to handle struct on heap
    cStubBuilder += std::format("static struct custom_operations {}_ops = ", nameLowerCase);
    cStubBuilder += "{\n";
    {
        StringOffsetHelper helper{cStubBuilder};
        cStubBuilder += std::format(".identifier = \"{}\",\n", sd.name);
        cStubBuilder += ".finalize = custom_finalize_default,\n";
        cStubBuilder += ".compare = custom_compare_default,\n";
        cStubBuilder += ".hash = custom_hash_default,\n";
        cStubBuilder += ".serialize = custom_serialize_default,\n";
        cStubBuilder += ".deserialize = custom_deserialize_default,\n";
        cStubBuilder += ".compare_ext = custom_compare_ext_default\n";
    }
    cStubBuilder += "};\n\n";

    // Struct creation
    cStubBuilder += std::format("CAMLprim value caml_{}_create()\n", nameLowerCase);
    cStubBuilder += "{\n";
    {
        StringOffsetHelper helper{cStubBuilder};
        cStubBuilder += "CAMLparam0();\n";
        cStubBuilder += "CAMLlocal1(v);\n";
        cStubBuilder += std::format("struct {} *p{} = caml_stat_alloc(sizeof *p{});\n", sd.name, nameLowerCase, nameLowerCase);
        // TODO: Calculate properly external memory and upper bound
        cStubBuilder += std::format("v = caml_alloc_custom(&{}_ops, sizeof(struct {} *), 0, 1);\n",nameLowerCase, sd.name);
        cStubBuilder += std::format("{}_val(v) = p{};\n",sd.name, nameLowerCase);
        cStubBuilder += "CAMLreturn(v);\n";
    }
    cStubBuilder += "}\n\n";


    for(const auto& field : sd.fields)
    {
        std::optional<std::string> optFieldHandle = std::invoke([&](){
            return GetTypeHandle(ast, field.typeNoPointer.value_or(field.t));
        });

        std::optional<std::string> optFieldHandleToCaml = std::invoke([&](){
            return TypeToOcamlTypeHandle(ast, field.typeNoPointer.value_or(field.t));
        });

        assert(optFieldHandle.has_value());
        assert(optFieldHandleToCaml.has_value());

        // Getter
        StringBuilder getterBuilder{1024};
        getterBuilder += std::format("CAMLprim value caml_{}_{}_get(value root)\n", sd.name, field.name);
        getterBuilder += "{\n";
        {
            StringOffsetHelper helper{getterBuilder};
            getterBuilder += "CAMLparam1(root);\n";
            getterBuilder += "CAMLlocal1(toReturn);\n";
            getterBuilder += std::format("{}* pRootType = {}_val(root);\n", sd.name, sd.name);
            getterBuilder += std::format("toReturn = (pRootType->{});\n", field.name);
            getterBuilder += std::format("CAMLreturn(toReturn);\n");
        }
        getterBuilder += "}\n\n";
        cStubBuilder += (getterBuilder.GetFinalString());

        // Setter
        StringBuilder setterBuilder{1024};
        setterBuilder += std::format("CAMLprim value caml_{}_{}_set(value root, value toSet)\n", sd.name, field.name);
        setterBuilder += "{\n";
        {
            StringOffsetHelper helper{setterBuilder};
            setterBuilder += "CAMLparam2(root, toSet);\n";
            setterBuilder += std::format("{}* pRootFromVal = {}_val(root);\n", sd.name, sd.name);
            setterBuilder += std::format("pRootFromVal->{} = {}(toSet);\n", field.name, *optFieldHandle);
            setterBuilder += "CAMLreturn(Val_unit);\n";
        }
        setterBuilder += "}\n\n";
        cStubBuilder += (setterBuilder.GetFinalString());
    }

    return cStubBuilder.GetFinalString();
}

static std::string SerializeNodeToCStub(const AST& ast, const ASTNode& node)
{
    StringBuilder cStubBuilder{1024};

    std::visit(overloaded{
        [&](const FunctionDecl& fd) -> void 
        {
            cStubBuilder.Assign(SerializeFunctionDeclToCStub(ast, fd));
        },
        [&](const StructDecl& sd) -> void
        {
            cStubBuilder.Assign(SerializeStructDeclToCStub(ast, sd));
        },
        [&](const UnionDecl& ) -> void
        {
        },
        [&](const TypeDefDecl& ) -> void
        {
        },
        [&](const EnumDecl&) -> void
        {
        }
    }, node);

    return cStubBuilder.GetFinalString();
}


// ********************************************************************************

// static void PrintASTNode(const ASTNode& node)
// {
//     std::visit(overloaded{
//         [](const FunctionDecl& fd) -> void
//         {
//             std::cout << std::format("Functiondecl - Type: {},Name: {},", fd.funcType, fd.funcName) << std::endl;
//             for(const auto& param : fd.params)
//                 std::cout << std::format("param name:{},", param.name);
//             std::cout << std::endl;
//         },
//         [](const StructDecl& sd) -> void
//         {
//             std::cout << std::format("Structdecl - Name: {},", sd.name) << std::endl;
//             for(const auto& field: sd.fields)
//                 std::cout << std::format("field name:{},", field.name);
//             std::cout << std::endl;
//         },
//         [](const auto&){}
//     }, node);
// }

static CXChildVisitResult visitFunctionArguments(CXCursor arg, CXCursor, CXClientData data) noexcept
{
    std::vector<Field>* fields = reinterpret_cast<std::vector<Field>*>(data);
    assert(fields != nullptr);

    switch(clang_getCursorKind(arg)) {
        case CXCursor_TypeRef:
            return CXChildVisit_Continue;
        case CXCursor_ParmDecl:
        {
            CXType t = clang_getCursorType(arg);
            CXString ts = clang_getTypeSpelling(t);
            CXString an = clang_getCursorSpelling(arg);

            if(t.kind == CXType_Pointer)
            {
                CXType pointee = clang_getPointeeType(t);
                CXType canon = clang_getCanonicalType(pointee);
                CXString name = clang_getTypeSpelling(canon);

                NameTypePair ntp;
                ntp.name = clang_getCString(an);
                ntp.t = clang_getCString(ts);
                ntp.typeNoPointer = std::optional<std::string>(clang_getCString(name));
                StringUtil::RemoveStruct(ntp.typeNoPointer.value());
                fields->emplace_back(std::move(ntp));
                clang_disposeString(name);
            }
            else 
            {
                CXType canon = clang_getCanonicalType(t);
                CXString canonName = clang_getTypeSpelling(canon);
                fields->emplace_back(clang_getCString(an), clang_getCString(canonName));
                clang_disposeString(canonName);
            }

            clang_disposeString(ts);
            clang_disposeString(an);
            return CXChildVisit_Continue;
        }

        default: {
            // CXType t = clang_getCursorType(arg);
            CXString an = clang_getCursorKindSpelling(clang_getCursorKind(arg));

            std::cout << "Unrecognized: " << clang_getCString(an) << std::endl;
            clang_disposeString(an);
            return CXChildVisit_Break;
        }
    }
}


static CXChildVisitResult visitStructFields(CXCursor fieldCursor, CXCursor, CXClientData data) noexcept
{
    std::vector<Field>* fields = reinterpret_cast<std::vector<Field>*>(data);
    assert(fields != nullptr);

    switch(clang_getCursorKind(fieldCursor)) {
        case CXCursor_FieldDecl:
        {
            CXType t = clang_getCursorType(fieldCursor);
            CXType canonType = clang_getCanonicalType(t);
            CXString ts = clang_getTypeSpelling(canonType);
            CXString an = clang_getCursorSpelling(fieldCursor);


            Field f;
            f.name = clang_getCString(an);
            f.t = clang_getCString(ts);
            
            if(canonType.kind == CXType_Pointer)
            {
                CXType pointee = clang_getPointeeType(canonType);
                CXType canon = clang_getCanonicalType(pointee);
                CXCursor decl = clang_getTypeDeclaration(canon);

                CXString name = clang_getCursorSpelling(decl);
                f.typeNoPointer = clang_getCString(name);
                clang_disposeString(name);
            }

            fields->emplace_back(std::move(f));

            clang_disposeString(ts);
            clang_disposeString(an);
            return CXChildVisit_Continue;
        }

        default: return CXChildVisit_Break;
    }
}


constexpr std::array<const char*, 2> argsForParse = {
    "-I.",
    "--target=x86_64-w64-windows-gnu"
};

int main(int, const char**)
{
    // Create clang index
    CXIndex index = clang_createIndex(0, 0);


    // Parse translation unit
    CXTranslationUnit tu = clang_parseTranslationUnit(
        index,
        "test.h",
        argsForParse.data(),
        1,
        nullptr, 0,
        CXTranslationUnit_None
    );

    if (!tu) {
        std::cerr << "Failed to parse translation unit\n";
        return 1;
    }

    std::cout << "Parsed translation unit successfully\n";

    // Get root cursor
    CXCursor cursor = clang_getTranslationUnitCursor(tu);

    AST ast;

    // Visitor: print function declarations
    // TODO: Process result after finishing parsing, right now it's parsing AND processing
    clang_visitChildren(
        cursor,
        [](CXCursor c, CXCursor, CXClientData pAst) {
            AST* pAstCasted = reinterpret_cast<AST*>(pAst);
            assert(pAstCasted != nullptr);
            AST& ast = *pAstCasted;
            switch(clang_getCursorKind(c)) {
                case CXCursor_FunctionDecl:
                {
                    FunctionDecl functionDecl; 
                    functionDecl.params.reserve(10);

                    CXString name = clang_getCursorSpelling(c);

                    CXType funcType = clang_getCursorType(c);
                    CXType returnType = clang_getResultType(funcType);
                    CXType returnTypeCanon = clang_getCanonicalType(returnType);
                    CXString funcTypeInStr = clang_getTypeSpelling(returnTypeCanon);

                    functionDecl.funcName = clang_getCString(name);
                    functionDecl.funcType = clang_getCString(funcTypeInStr);

                    if(returnTypeCanon.kind == CXType_Pointer)
                    {
                        CXType pointee = clang_getPointeeType(returnTypeCanon);
                        CXType canon = clang_getCanonicalType(pointee);
                        CXCursor decl = clang_getTypeDeclaration(canon);

                        CXString name = clang_getCursorSpelling(decl);
                        functionDecl.returnTypeNoPointer = clang_getCString(name);
                        clang_disposeString(name);
                    }

                    clang_visitChildren(c, &visitFunctionArguments, &functionDecl.params);

                    ASTNode node = std::move(functionDecl);
                    std::cout << SerializeNodeToOCamlStub(ast, node) << std::endl;
                    std::cout << SerializeNodeToCStub(ast, node) << std::endl;

                    PushNodeToAST(ast, std::move(node));
                    clang_disposeString(funcTypeInStr);
                    clang_disposeString(name);
                    break;
                }
                case CXCursor_StructDecl:
                {
                    StructDecl structDecl; 
                    structDecl.fields.reserve(10);

                    CXString name = clang_getCursorSpelling(c);
                    structDecl.name= clang_getCString(name);
                    clang_visitChildren(c, &visitStructFields, &structDecl.fields);

                    ASTNode node = std::move(structDecl);

                    std::cout << SerializeNodeToOCamlStub(ast, node) << std::endl;
                    std::cout << SerializeNodeToCStub(ast, node) << std::endl;

                    PushNodeToAST(ast, std::move(node));
                    clang_disposeString(name);
                    break;
                }
                case CXCursor_MacroDefinition:
                {
                    // StructDecl structDecl; 
                    // structDecl.fields.reserve(10);

                    // CXString name = clang_getCursorSpelling(c);
                    // structDecl.name= clang_getCString(name);
                    // clang_visitChildren(c, &visitStructFields, &structDecl.fields);

                    // ASTNode node = std::move(structDecl);
                    // PrintASTNode(node);

                    // clang_disposeString(name);
                    assert(false && "Will do soon");
                    break;
                }

                case CXCursor_EnumDecl:
                default: break;
            }

            return CXChildVisit_Continue;
        },
        &ast
    );

    unsigned n = clang_getNumDiagnostics(tu);
    for (unsigned i = 0; i < n; i++) {
      CXDiagnostic d = clang_getDiagnostic(tu, i);
      CXString s =
          clang_formatDiagnostic(d, clang_defaultDiagnosticDisplayOptions());
      std::cerr << clang_getCString(s) << std::endl;
      clang_disposeString(s);
      clang_disposeDiagnostic(d);
    }

    // Cleanup
    clang_disposeTranslationUnit(tu);
    clang_disposeIndex(index);

    return 0;
}