describe "olegdb client can", ->
  it "be imported", ->
    require "olegdb"

    assert.truthy olegdb

  it "create a database object", ->
    assert true
