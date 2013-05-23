solution = Solution.new("kfx")
project = Project.new("kfx")

solution:cmd()

project:addExclude(".git/**")
project:addExclude("build/**")

project:addFile("Sources/**")

if platform == Platform.Windows then
	project:addIncludeDir("Libraries/DirectX/Include")
	project:addLibFor("Win32", "Libraries/DirectX/Lib/dxguid")
	project:addLibFor("Win32", "Libraries/DirectX/Lib/d3dx9")
	project:addLibFor("Win32", "Libraries/DirectX/Lib/d3d11")
	project:addLibFor("Win32", "Libraries/DirectX/Lib/d3dcompiler")
end

project:addSubProject(Solution.createProject("angleproject"))

solution:addProject(project)