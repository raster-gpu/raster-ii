all: $(BUILDDIR)/raster.bit

$(BUILDDIR)/raster.json: raster.tcl $(BUILDDIR)/../Raster.sv top.v tmds_encoder.v
	@yosys -c $<

$(BUILDDIR)/raster.config: $(BUILDDIR)/raster.json ulx3s.lpf
	@nextpnr-ecp5 \
		--85k \
		--json $(word 1,$^) \
		--lpf $(word 2,$^) \
		--textcfg $@

$(BUILDDIR)/raster.bit: $(BUILDDIR)/raster.config
	@ecppack $< $@ --compress

prog: $(BUILDDIR)/raster.bit
	@openFPGALoader -b ulx3s $(BUILDDIR)/raster.bit

flash: $(BUILDDIR)/raster.bit
	@openFPGALoader -b ulx3s -f --unprotect-flash $(BUILDDIR)/raster.bit
