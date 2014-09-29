require("colorize")
require("pathname")
require("find")

task :default => [:install]

# Recreates the directory structure from rootDir in targetDir.
# Any files found within will be sym_linked
def linkDirContents(rootDir, targetDir)
	rootDir = Pathname.new(rootDir)
	targetDir = Pathname.new(targetDir)

	# Recurisvely visit all files in the rootDir
	Find.find(rootDir) { |srcFullPath|
		srcFullPath = Pathname.new(srcFullPath)
		relativePath = srcFullPath.relative_path_from(rootDir)

		case relativePath.to_s
		when '.'
			next

		# ignore git directory and children
		when '.git'
			Find.prune
		end

		# Skip this rake file
		next if srcFullPath.to_s == __FILE__

		targetFullPath = targetDir + relativePath

		if srcFullPath.directory?
			puts "making dir: #{targetFullPath}"
			FileUtils.mkdir_p(targetFullPath)
		end

		puts "#{srcFullPath} => #{targetFullPath}"
		FileUtils.ln_sf(srcFullPath, targetFullPath)
	}
end

task :install do
	puts("Installing dotfiles...".green)

	rootDir = Pathname.new(File.dirname(__FILE__))
	targetDir = Pathname.new(Dir.home)

	linkDirContents(rootDir, targetDir)

	puts("\nDone!".green)
end
