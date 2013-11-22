solution = Solution.new("kfx")
project = Project.new("kfx")

solution:cmd()

project:addExclude(".git/**")
project:addExclude("build/**")

project:addFile("Sources/**")
project:addFile("glsl2agal/src/**")
project:addFile("glsl2agal/agalassembler/**")
project:addFile("glsl2agal/swc/**")
project:addExclude("glsl2agal/src/glsl/main.cpp")
project:addExclude("glsl2agal/src/glsl/*.ll")
project:addExclude("glsl2agal/src/glsl/*.yy")
project:addExclude("glsl2agal/src/glsl/*.sh")
project:addExclude("glsl2agal/src/glsl/glcpp/tests/**")
project:addExclude("glsl2agal/src/glsl/glcpp/glcpp.c")
project:addExclude("glsl2agal/src/glsl/builtin_stubs.cpp")

project:addIncludeDir("glsl2agal/src/glsl")
project:addIncludeDir("glsl2agal/src/mesa")
project:addIncludeDir("glsl2agal/include")
if platform == Platform.Windows then
	project:addIncludeDir("glsl2agal/msinttypes-r26")
end

project:addDefine("GLSL2AGAL_LIB")

if platform == Platform.Windows then
	project:addIncludeDir("Libraries/DirectX/Include")
	project:addLibFor("Win32", "Libraries/DirectX/Lib/dxguid")
	project:addLibFor("Win32", "Libraries/DirectX/Lib/d3dx9")
	project:addLibFor("Win32", "Libraries/DirectX/Lib/d3d11")
	project:addLibFor("Win32", "Libraries/DirectX/Lib/d3dcompiler")
end

project:addSubProject(Solution.createProject("angleproject"))

solution:addProject(project)