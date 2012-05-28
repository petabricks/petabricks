PBC=../../src/pbc

.PHONY: all clean test testVA testMetric testMetricVA testBinnedLR

all: nwkde nwkdeVA nwkdeMetric BinnedLR

clean:
	rm -f nwkde nwkdeVA nwkdeMetric BinnedLR

nwkde: nwkde.pbcc nwkdeGenerators.pbcc nwkdeMetric.pbcc utils.pbcc nwkde.h
	${PBC} $< --output $@

nwkdeVA: nwkdeVA.pbcc nwkde.pbcc nwkdeGenerators.pbcc nwkdeMetric.pbcc utils.pbcc nwkde.h
	${PBC} $< --output $@

nwkdeMetric: nwkdeMetric.pbcc nwkde.h
	${PBC} $< --output $@

KernelEstimate: KernelEstimate.pbcc LinearSolvePosDef.pbcc
	${PBC} $< --output $@

BinnedLR: BinnedLR.pbcc
	${PBC} $< --output $@

test: nwkde
	./nwkde test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/kernelWidths.pba test/split.pba test/maskWidth.pba -

testVA: nwkdeVA
	./nwkdeVA test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/split.pba test/maskWidth.pba -

testMetric: nwkdeMetric nwkde
	./nwkde test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/kernelWidths.pba test/split.pba test/maskWidth.pba testMetric.out.pba
	./nwkdeMetric testMetric.out.pba test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/kernelWidths.pba test/split.pba test/maskWidth.pba -
	rm testMetric.out.pba

testMetricVA: nwkdeMetric nwkdeVA
	./nwkdeVA test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/split.pba test/maskWidth.pba testMetricVA.out.pba
	./nwkdeMetric testMetricVA.out.pba test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/kernelWidths.pba test/split.pba test/maskWidth.pba -
	rm testMetricVA.out.pba

testBinnedLR: BinnedLR
	./BinnedLR btest/trainData.pba btest/x.pba btest/dir.pba btest/y.pba btest/indices.pba btest/testData.pba btest/x.pba btest/dir.pba btest/indices.pba ../../testdata/Zero0D ../../testdata/Zero0D btest/output.pba
	./BinnedLR btest/trainData.pba btest/x.pba btest/dir.pba btest/y.pba btest/indices.pba btest/testData.pba btest/x.pba btest/dir.pba btest/indices.pba ../../testdata/Ten0D ../../testdata/Twelve0D btest/output2.pba
