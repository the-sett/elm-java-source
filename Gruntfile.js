module.exports = function (grunt) {
    grunt.loadNpmTasks('grunt-contrib-clean');
    grunt.loadNpmTasks('grunt-contrib-copy');
    grunt.loadNpmTasks('grunt-contrib-connect');
    grunt.loadNpmTasks('grunt-contrib-compress');
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('grunt-contrib-uglify');
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-elm');
    grunt.loadNpmTasks('grunt-exec');

    grunt.initConfig({
        'pkg': grunt.file.readJSON('package.json'),

        'exec': {
            'elm-install': {
                command: 'elm-install'
            },
            'elm-install-offline': {
                command: 'elm-install --skip-update'
            },
            'closure': {
                command: './closure-minify'
            }
        },

        'elm': {
            compile: {
                files: {
                    'app/<%= pkg.name %>.js': ['src/elm/**/*.elm']
                }
            }
        },

        'copy': {
            'dist': {
                files: [
                    {expand: true, cwd: 'src', src: ['index.html'], dest: 'app'}
                ],
            }
        },

        'uglify': {
            'options': {
                'mangle': false
            },
            'dist': {
                'files': {
                    'app/<%= pkg.name %>.min.js': ['app/<%= pkg.name %>.js']
                }
            }
        },

        'compress': {
            dist: {
                options: {
                    archive: 'dist/<%= pkg.name %>-<%= pkg.version %>.zip'
                },
                files: [{
                    src: [ 'app/**' ],
                    dest: '/'
                }]
            }
        },

        'connect': {
            server: {
                options: {
                    hostname: 'localhost',
                    port: 9070,
                    base: 'app'
                }
            }
        },

        'watch': {
            'dev': {
                files: [ 'Gruntfile.js', 'elm-package.json', 'src/**' ],
                tasks: [ 'loop' ],
                options: {
                    atBegin: true
                }
            }
        },

        'clean': {
            temp: {
                src: [ 'tmp', 'app', 'dist', 'node_modules', 'elm-stuff' ]
            }
        },
    });

    grunt.registerTask('dev', ['connect:server', 'exec:elm-install-offline', 'loop', 'watch:dev']);
    grunt.registerTask('build', ['exec:elm-install', 'loop']);
    grunt.registerTask('loop', ['copy', 'elm' ]);
    grunt.registerTask('package', ['build', 'uglify', 'compress']);
};
