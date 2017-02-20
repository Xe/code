describe "the testing environment has environment variables set up", ->
    it "has the host variable", ->
      assert.truthy os.getenv "OLEG_HOST"
    it "has the port variable", ->
      assert.truthy os.getenv "OLEG_PORT"
    it "has the table name variable", ->
      assert.truthy os.getenv "OLEG_TABLE"
