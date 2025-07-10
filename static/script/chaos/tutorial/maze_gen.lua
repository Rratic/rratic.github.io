local module = {}

local function random_evenodd(left_bound, right_bound)
    local len = (right_bound - left_bound) >> 1
    return (math.random(len) << 1) + left_bound - 1
end

local function build_maze_fragment(matrix, xl, xr, yl, yr)
    local xlen = xr - xl
    local ylen = yr - yl
    if xlen == 0 or ylen == 0 then
        for i = xl, xr do
            for j = yl, yr do
                matrix:set(i, j, 1)
            end
        end
        return
    end

    local xm = random_evenodd(xl, xr)
    local ym = random_evenodd(yl, yr)
    -- for i = xl, xr do
    --     matrix:set(i, ym, 0)
    -- end
    -- for j = yl, yr do
    --     matrix:set(xm, j, 0)
    -- end

    build_maze_fragment(matrix, xl, xm - 1, yl, ym - 1)
    build_maze_fragment(matrix, xm + 1, xr, yl, ym - 1)
    build_maze_fragment(matrix, xl, xm - 1, ym + 1, yr)
    build_maze_fragment(matrix, xm + 1, xr, ym + 1, yr)

    local solid = math.random(4)
    if solid ~= 1 then
        matrix:set(random_evenodd(xl - 1, xm), ym, 1)
    end
    if solid ~= 2 then
        matrix:set(random_evenodd(xm, xr + 1), ym, 1)
    end
    if solid ~= 3 then
        matrix:set(xm, random_evenodd(yl - 1, ym), 1)
    end
    if solid ~= 4 then
        matrix:set(xm, random_evenodd(ym, yr + 1), 1)
    end
end

local function build_maze_chunk(matrix, x, y)
    matrix:fill(0)
    -- matrix.seed = (x, y)
    math.randomseed(x, y)
    for i = 0, 31 do
        matrix:set(0, i, 0)
        matrix:set(i, 0, 0)
    end
    build_maze_fragment(matrix, 1, 31, 1, 31)
    matrix:set(0, random_evenodd(2, 32), 1)
    matrix:set(random_evenodd(2, 32), 0, 1)
end

module.gen = build_maze_chunk

return module
