open Jest;
open Expect;

let x = 1;

describe("newGensym", () => {
  let gensym = Utils.newGensym();

  test("first call", () =>
    expect(gensym("abc")) |> toBe("abc1")
  );
  test("second call", () =>
    expect(gensym("def")) |> toBe("def2")
  );
});
