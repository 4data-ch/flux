package arrow

import (
	"github.com/apache/arrow/go/arrow"
	"github.com/apache/arrow/go/arrow/array"
	arrowmemory "github.com/apache/arrow/go/arrow/memory"
	"github.com/influxdata/flux/memory"
)

func NewString(vs []string, alloc *memory.Allocator) *array.Binary {
	b := NewStringBuilder(alloc)
	b.Reserve(len(vs))
	for _, v := range vs {
		b.AppendString(v)
	}
	a := b.NewBinaryArray()
	b.Release()
	return a
}

func NewStringBuilder(a *memory.Allocator) *array.BinaryBuilder {
	return array.NewBinaryBuilder(&allocator{
		Allocator: arrowmemory.NewGoAllocator(),
		alloc:     a,
	}, arrow.BinaryTypes.String)
}