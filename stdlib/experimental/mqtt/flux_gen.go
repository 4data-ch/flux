// DO NOT EDIT: This file is autogenerated via the builtin command.

package mqtt

import (
	flux "github.com/influxdata/flux"
	ast "github.com/influxdata/flux/ast"
)

func init() {
	flux.RegisterPackage(pkgAST)
}

var pkgAST = &ast.Package{
	BaseNode: ast.BaseNode{
		Errors: nil,
		Loc:    nil,
	},
	Files: []*ast.File{&ast.File{
		BaseNode: ast.BaseNode{
			Errors: nil,
			Loc: &ast.SourceLocation{
				End: ast.Position{
					Column: 11,
					Line:   3,
				},
				File:   "mqtt.flux",
				Source: "package mqtt\n\nbuiltin to",
				Start: ast.Position{
					Column: 1,
					Line:   1,
				},
			},
		},
		Body: []ast.Statement{&ast.BuiltinStatement{
			BaseNode: ast.BaseNode{
				Errors: nil,
				Loc: &ast.SourceLocation{
					End: ast.Position{
						Column: 11,
						Line:   3,
					},
					File:   "mqtt.flux",
					Source: "builtin to",
					Start: ast.Position{
						Column: 1,
						Line:   3,
					},
				},
			},
			ID: &ast.Identifier{
				BaseNode: ast.BaseNode{
					Errors: nil,
					Loc: &ast.SourceLocation{
						End: ast.Position{
							Column: 11,
							Line:   3,
						},
						File:   "mqtt.flux",
						Source: "to",
						Start: ast.Position{
							Column: 9,
							Line:   3,
						},
					},
				},
				Name: "to",
			},
		}},
		Imports: nil,
		Name:    "mqtt.flux",
		Package: &ast.PackageClause{
			BaseNode: ast.BaseNode{
				Errors: nil,
				Loc: &ast.SourceLocation{
					End: ast.Position{
						Column: 13,
						Line:   1,
					},
					File:   "mqtt.flux",
					Source: "package mqtt",
					Start: ast.Position{
						Column: 1,
						Line:   1,
					},
				},
			},
			Name: &ast.Identifier{
				BaseNode: ast.BaseNode{
					Errors: nil,
					Loc: &ast.SourceLocation{
						End: ast.Position{
							Column: 13,
							Line:   1,
						},
						File:   "mqtt.flux",
						Source: "mqtt",
						Start: ast.Position{
							Column: 9,
							Line:   1,
						},
					},
				},
				Name: "mqtt",
			},
		},
	}},
	Package: "mqtt",
	Path:    "experimental/mqtt",
}
