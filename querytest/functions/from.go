package functions

import (
	"fmt"

	"github.com/influxdata/flux"
	"github.com/influxdata/flux/functions/inputs"
	"github.com/influxdata/flux/functions/transformations"
	"github.com/influxdata/flux/plan"
	"github.com/influxdata/flux/semantic"
)

// Registers testing-specific procedure spec and rules.
func init() {
	// This signature is just for compatibility with FromOpSpec.
	// Conceptually, the mock from could have no parameter.
	mockFromSignature := semantic.FunctionPolySignature{
		Parameters: map[string]semantic.PolyType{
			"bucket":   semantic.String,
			"bucketID": semantic.String,
		},
		Required: nil,
		Return:   flux.TableObjectType,
	}

	flux.RegisterFunction(inputs.FromKind, createMockFromOpSpec, mockFromSignature)
	plan.RegisterProcedureSpec(inputs.FromKind, newMockFromProcedure, inputs.FromKind)
	plan.RegisterPhysicalRules(MergeMockFromRangeRule{})
}

func createMockFromOpSpec(args flux.Arguments, a *flux.Administration) (flux.OperationSpec, error) {
	spec := new(inputs.FromOpSpec)

	if bucket, ok, err := args.GetString("bucket"); err != nil {
		return nil, err
	} else if ok {
		spec.Bucket = bucket
	} else {
		spec.Bucket = "testBucket"
	}

	if bucketID, ok, err := args.GetString("bucketID"); err != nil {
		return nil, err
	} else if ok {
		spec.BucketID = bucketID
	}

	return spec, nil
}

// This procedure spec is used in flux tests to represent the physical
// (yet storage-agnostic) counterpart of the logical `from` operation.
// A physical (yet mocked) representation is necessary to make physical planning succeed,
// and to make queries containing `from` compile successfully.
type MockFromProcedureSpec struct {
	plan.DefaultCost

	Bounded bool
}

func newMockFromProcedure(qs flux.OperationSpec, pa plan.Administration) (plan.ProcedureSpec, error) {
	_, ok := qs.(*inputs.FromOpSpec)
	if !ok {
		return nil, fmt.Errorf("invalid spec type %T", qs)
	}

	return new(MockFromProcedureSpec), nil
}

func (MockFromProcedureSpec) Kind() plan.ProcedureKind {
	return inputs.FromKind
}

func (s *MockFromProcedureSpec) Copy() plan.ProcedureSpec {
	ns := new(MockFromProcedureSpec)
	ns.Bounded = s.Bounded
	return ns
}

func (s MockFromProcedureSpec) PostPhysicalValidate(id plan.NodeID) error {
	if !s.Bounded {
		return fmt.Errorf(`%s: results must be bounded`, id)
	}
	return nil
}

// MergeFromRangeRule pushes a `range` into a mock `from`.
// It doesn't compute any bound, but it makes the `from` bounded in order to pass physical validation.
// A mock `from` operation should exists only for testing purpose.
type MergeMockFromRangeRule struct{}

func (rule MergeMockFromRangeRule) Name() string {
	return "MergeMockFromRangeRule"
}

func (rule MergeMockFromRangeRule) Pattern() plan.Pattern {
	return plan.Pat(transformations.RangeKind, plan.Pat(inputs.FromKind))
}

func (rule MergeMockFromRangeRule) Rewrite(node plan.PlanNode) (plan.PlanNode, bool, error) {
	from := node.Predecessors()[0]
	fromSpec, ok := from.ProcedureSpec().(*MockFromProcedureSpec)

	// Check that this is a MockFrom and not another from.
	// This shouldn't happen, because this rule should be registered only for tests,
	// and MockFrom should be the only registered procedure for from.
	if !ok {
		return node, false, fmt.Errorf("%s: from is not mocked, cannot rewrite", rule.Name())
	}

	fromRange := fromSpec.Copy().(*MockFromProcedureSpec)

	fromRange.Bounded = true

	// merge nodes into single operation
	merged, err := plan.MergeToPhysicalPlanNode(node, from, fromRange)
	if err != nil {
		return nil, false, err
	}

	return merged, true, nil
}