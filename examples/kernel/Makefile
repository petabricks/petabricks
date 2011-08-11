PBC=../../src/pbc

.PHONY: all test

all: nwkde

nwkde: nwkde.pbcc nwkdeGenerators.pbcc nwkdeMetric.pbcc utils.pbcc nwkde.h
	${PBC} $< --output $@

nwkdeVA: nwkdeVA.pbcc nwkde.pbcc nwkdeGenerators.pbcc nwkdeMetric.pbcc utils.pbcc nwkde.h
	${PBC} $< --output $@

KernelEstimate: KernelEstimate.pbcc LinearSolvePosDef.pbcc
	${PBC} $< --output $@

test: nwkde
	./nwkde test/trainData.pba test/wrapFlags.pba test/kernelWidths.pba test/trainIndices.pba test/trainVarIndex.pba test/trainTimeOffset.pba test/testData.pba test/testIndices.pba test/testVarIndex.pba test/testTimeOffset.pba test/outputVarIndex.pba test/outputTimeOffset.pba test/trainMaskWidth.pba -

testVA: nwkdeVA
	./nwkdeVA test/trainData.pba test/wrapFlags.pba test/trainIndices.pba test/trainVarIndex.pba test/trainTimeOffset.pba test/testData.pba test/testIndices.pba test/testVarIndex.pba test/testTimeOffset.pba test/outputVarIndex.pba test/outputTimeOffset.pba test/trainMaskWidth.pba -

