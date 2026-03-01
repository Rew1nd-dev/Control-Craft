## 弹道计算/预测数值计算教学

### 前言

create big cannons mod一直是瓦尔基里社区最受欢迎的模组之一，其中实现了若干
机制丰富又富有趣味的火炮与炮弹。在玩家游玩过程中，常常遇到需要对弹道进行解析、
对目标进行预测瞄准、对远距离目标进行精准打击等需求，本教程旨在针对create big
cannons中的常见弹道（线性阻尼抛物运动弹头），在己方与目标均进行运动的情况下，
计算最佳瞄准方向的问题进行详细讲解。

基于本教程提供的方法，读者可以自行将方法推广到其他特殊弹道的场景，或弱化场景
到静对动，静对静进行应用

### 弹道的解析形式

create big cannons中的弹道是由大约1秒执行20次的离散迭代步骤产生的，我们将要
讨论的弹道（以下简称“标准弹道”）是炮弹发射后，在一个线性速度阻尼系数$c$和一个重力
系数$g$作用下迭代产生的，其迭代式为：


$$
\pmb{v}_{n+1} = (1 - c) \pmb{v}_{n} + \pmb{g}
$$
$$
\pmb{x}_{n+1} = \pmb{x}_{n} + \pmb{v}_n
$$

其中：
- $c$ 为线性阻尼系数（空阻）。
- $\pmb{g}$ 为重力向量。
- $n$ 为迭代步数（tick）。

### 弹道的解析推导

通过对离散迭代式进行解析求解，我们可以直接计算 $t$ 个 tick 后的总位移。这种方法称为前向欧拉积分。

#### 1. 速度公式推导
速度递推式是一个一阶非齐次线性差分方程。其解由齐次解和特解组成：
$$ \pmb{v}_n = (1-c)^n \pmb{v}_0 + \frac{1 - (1-c)^n}{c} \pmb{g} $$

#### 2. 位移公式推导
根据位移递推式（前向欧拉）：
$$ \pmb{x}_n = \pmb{x}_0 + \sum_{i=0}^{n-1} \pmb{v}_i $$

利用等比数列求和公式 $\sum_{i=0}^{n-1} a^i = \frac{1-a^n}{1-a}$，对速度公式进行累加：
$$ \Delta \pmb{x} = \sum_{i=0}^{n-1} \left[ (1-c)^i \pmb{v}_0 + \frac{1 - (1-c)^i}{c} \pmb{g} \right] $$
$$ \Delta \pmb{x} = \frac{1 - (1-c)^n}{c} \pmb{v}_0 + \left[ \frac{n}{c} - \frac{1 - (1-c)^n}{c^2} \right] \pmb{g} $$



#### 3. 结论

为了后续数值计算便利，能计算炮弹在任意时刻的位移，同时使用m/s和s作为速度和时间的单位而不是tick，现将上述结果改写成lua代码：

```lua
-- v 初速度，为Vector3d类型, 单位为m/s
-- t 飞行时间，单位为s
-- c, g依旧为弹道参数，不需要考虑量纲，查询mod数据获取
function trajectory(v, t, c, g) 
    -- 将v转换为tick单位
    v = v:mul(0.05)
    t = t * 20

    local a = 1 - c
    local x0 = v.x
    local y0 = v.y
    local z0 = v.z
    local x = x0 * (1 - a^t) / c + 0.5 * g * t
    local y = y0 * (1 - a^t) / c + g/c * t + 0.5 * g * t
    local z = z0 * (1 - a^t) / c + 0.5 * g * t
    -- 返回单位为m的位移矢量
    return Vector3d.new(x, y, z)
end
```
对于其他任意特殊类型的弹道，只要能够求出炮弹位移矢量$x$随时间$t$的变化关系，即可利用以下提到的方法进行数值计算。

### 计算瞄准方向矢量

#### 1. 符号规定
矢量符号中，第一个下标w表示世界坐标系，s表示船体坐标系，s2w表示船体坐标系到世界坐标系的转换关系
- 现在考虑己方炮塔所在船体质心在世界坐标系中位置为$x_{w,s}$
- 船体具有一定的旋转，将矢量从船体坐标系转换到世界坐标系的四元数为$q_{s2w}$
- 炮塔在船体坐标系中相对质心具有位移$o_{s}$,
- 炮塔长度为$l_{b}$
- 船具有速度$v_{w,s}$, 角速度$\omega_{w,s}$
- 炮塔的待求偏航角为$\theta$, 仰角为$\phi$，炮弹飞行时间为$t$
- 目标在世界坐标系中的位置为$x_{w,t,0}$，速度为$v_{w,t,0}$

#### 2. 炮弹轨迹方程

炮弹发射时，其世界位置为：
$$ \pmb{s(θ,φ)} = [cos(θ)cos(φ), sin(φ), sin(θ)cos(φ)]^T $$
$$ o_w = R(q_{s2w})(o_s + l_b * \pmb{s(θ,φ)}) $$
$$ x_{w,b,0} = x_{w,s} + o_w $$
炮弹的发射初速度为：
$$ v_{w,b,0} = R(q_{s2w})(v_b * \pmb{s(θ,φ)}) + v_{w,s} + \omega_{w,s} \times o_w $$

其中：
- $x_{w,s}$ 为船体质心在世界坐标系中的位置。
- $R(q_{s2w})$ 为将船体坐标系转换到世界坐标系的旋转矩阵。
- $v_b$ 为炮弹的初速度，典型值为160m/s

炮弹在$t$时刻的位置为：
$$ x_{w,b}(t) = x_{w,b,0} + trajectory(v_{w,b,0}, t, c, g) $$

#### 3. 目标轨迹外推预测

需要指出的是，预测目标未来位置有诸多手段，如假设目标作圆周运动，或是
匀加速运动等等，我们现在考虑最简单最常用的预测，即假设目标作匀速直线运动：
$$ x_{w,t}(t) = x_{w,t,0} + v_{w,t,0} * t $$

#### 4. 最优化模型

假设炮弹在$t$时刻击中目标，则有：
$$ x_{w,b}(t) = x_{w,t}(t) $$
$$ x_{w,b,0} + trajectory(v_{w,b,0}(θ,φ), t, c, g) - (x_{w,t,0} + v_{w,t,0} * t) = 0$$

除去已知量，上述等式中我们需要求解三个未知量：
- 炮弹飞行时间$t$
- 炮塔偏航角$\\theta$
- 炮塔仰角$\\phi$

这绝大多数情况下是一个非线性方程组，无法直接求解，因此我们需要使用数值方法来求解。

定义目标函数 $\pmb{f}(t, \theta, \phi)$ 为炮弹与目标在 $t$ 时刻的距离矢量：
$$ \pmb{f}(t, \theta, \phi) = \pmb{x}_{w,b}(t, \theta, \phi) - \pmb{x}_{w,t}(t) $$

我们的目标是找到一组参数 $\pmb{p} = [t, \theta, \phi]^T$，使得目标函数的模长最小，即求解最小二乘问题：
$$ \min_{\pmb{p}} \frac{1}{2} \|\pmb{f}(\pmb{p})\|^2 $$

#### 2. Levenberg-Marquardt (LM) 算法原理

Levenberg-Marquardt 算法是一种非线性最小二乘优化算法，它结合了 **高斯-牛顿法 (Gauss-Newton)** 和 **梯度下降法 (Gradient Descent)** 的优点：
- 当 $\pmb{p}$ 距离最优解较远时，表现得像梯度下降法，步长较小但保证下降。
- 当 $\pmb{p}$ 接近最优解时，表现得像高斯-牛顿法，具有二阶收敛速度。

LM 算法的迭代步长 $\pmb{h}$ 通过求解以下线性方程组获得：
$$ (\pmb{J}^T \pmb{J} + \lambda \pmb{I}) \pmb{h} = -\pmb{J}^T \pmb{f} $$

其中：
- $\pmb{J}$ 是目标函数 $\pmb{f}$ 的雅可比矩阵（Jacobian Matrix），$\pmb{J}_{ij} = \frac{\partial f_i}{\partial p_j}$。
- $\lambda$ 是阻尼系数。较大的 $\lambda$ 倾向于梯度下降，较小的 $\lambda$ 倾向于高斯-牛顿。
- $\pmb{I}$ 是单位矩阵。

#### 3. 求解步骤

1.  **初始猜测**：给定初始时间 $t_0$（根据距离和初速粗略估计）、偏航角 $\theta_0$ 和仰角 $\phi_0$。
2.  **计算残差与雅可比**：
    - 计算当前残差 $\pmb{f}(\pmb{p})$。
    - 由于弹道方程较为复杂，雅可比矩阵 $\pmb{J}$ 通常使用 **有限差分法** 估算：
      $$ \frac{\partial f_i}{\partial p_j} \approx \frac{f_i(p_j + h) - f_i(p_j - h)}{2h} $$
3.  **构造并求解方程**：构造正规方程 $(\pmb{J}^T \pmb{J} + \lambda \pmb{I}) \pmb{h} = -\pmb{J}^T \pmb{f}$ 并解出变化量 $\pmb{h}$。
4.  **尝试更新参数**：计算 $\pmb{p}_{new} = \pmb{p} + \pmb{h}$。
    - 如果 $f(\pmb{p}_{new})$ 的误差小于当前误差，则接受更新，并减小 $\lambda$ 以加速收敛。
    - 如果误差反而变大，则拒绝更新，增大 $\lambda$ 并重新寻找。
5.  **迭代终止条件**：当残差足够小、更新量极小或达到最大迭代次数时，停止计算。

通过这种数值迭代，我们在短时间内得到极高精度的解。

## 参考实现

基于 `ballistics.lua` 中 `loop()` 函数的执行流程，实现拦截计算可分为以下四个核心步骤：

### 1. 目标采样与轨迹预测

首先，我们需要通过采样器收集目标的位置与速度，并构造一个随时间变化的预测函数。

```lua
-- (1) 数据采样：在计时循环中持续推送目标状态
local ts = 0.01667 -- physics thread
-- 简单的采样器，本质是一个固定长度的队列
local sampler = {
    k = 5,
    samples = {},
    time = 0,
    push = function (self, p, v)
        self.time = self.time + ts
        table.insert(self.samples, {p = p, v = v, t = self.time})
        while #self.samples > self.k do
            table.remove(self.samples, 1)
        end
    end
}

sampler:push(target_pos, target_vel)
-- 简单线性外推，可替换成其他预测函数
function Ballistics.trajectoryLinear(samples)
    local last = samples[#samples]
    if not last or not last.v then return nil end
    
    local p0 = last.p
    local v0 = last.v
    local t0 = last.t
    
    return function(t)
        return p0:add(v0:mul(t))
    end
end

-- (2) 轨迹外推：基于采样结果生成预测函数 targetFunc(t)，注意返回的是函数而不是值。
-- 常用线性预测：P(t) = P_last + V_last * t
local targetFunc = Ballistics.trajectoryLinear(sampler.samples)
```

### 2. 构建炮弹轨迹方程

```lua
function aimPredictLM(
        shooter,        -- { pos, vel, omega, rotation }，自身船参数
        gun,            -- { mount_offset, barrel_length, muzzle_vel }，炮弹参数
        target_trajectory,    -- function(t) -> Vector3d (目标位置)
        projectile_trajectory,-- function(t, v_total) -> Vector3d (炮弹相对炮口的位移)
        maxIterations, tol, lambda, h -- 求解器参数
)
    -- 启发式初始化
    -- 1. 估计目标速度
    local p_target_0 = target_trajectory(0)
    local p_target_next = target_trajectory(0.1)
    local v_target_est = p_target_next:sub(p_target_0):div(0.1)
    
    -- 2. 迭代线性拦截（5次迭代）
    -- 假设炮弹速度和目标速度恒定
    local p_muzzle_est_world = shooter.pos:add(shooter.rotation:transform(gun.mount_offset))
    local muzzle_speed = gun.muzzle_vel
    if muzzle_speed < 1.0 then muzzle_speed = 1.0 end
    
    local t_est = 0.1
    local p_aim = p_target_0
    
    for _ = 1, 5 do
        local dist = p_aim:distance(p_muzzle_est_world)
        t_est = dist / muzzle_speed
        -- 更新瞄准点
        p_aim = p_target_0:add(v_target_est:mul(t_est))
    end
    
    if t_est < 0.01 then t_est = 0.01 end
    
    -- 3. 计算猜测值
    local diff = p_aim:sub(p_muzzle_est_world)
    local dir_world_est = diff:normalize()
    local dir_local_est = shooter.rotation:conjugate():transform(dir_world_est)
    
    local yaw_0 = atan2(dir_local_est.x, dir_local_est.z)
    local pitch_0 = math.asin(clamp(dir_local_est.y / (1e-6 + dir_local_est:length()), 1.0))
    local t_0 = t_est

    local function loss(yaw, pitch, t)
        -- 1. 计算炮弹运动学
        local dir_local = Ballistics.toView(yaw, pitch)
        local dir_world = shooter.rotation:transform(dir_local):normalize()
        
        -- 炮口位置
        local p_pivot_world = shooter.pos:add(shooter.rotation:transform(gun.mount_offset))
        local p_muzzle_world = p_pivot_world:add(dir_world:mul(gun.barrel_length))
        
        -- 炮口速度
        local r_muzzle = p_muzzle_world:sub(shooter.pos)
        local v_tangential = shooter.omega:cross(r_muzzle)
        local v_muzzle_world = shooter.vel:add(v_tangential):add(dir_world:mul(gun.muzzle_vel))
        
        -- 2. 炮弹轨迹
        local displacement = projectile_trajectory(t, v_muzzle_world)
        local p_proj = p_muzzle_world:add(displacement)
        
        -- 3. 目标位置
        local p_target = target_trajectory(t)
        
        -- 4. 计算残差
        return {
            p_proj.x - p_target.x,
            p_proj.y - p_target.y,
            p_proj.z - p_target.z
        }
    end

    -- 投影步，保证解的合理性
    local function projector(yaw, pitch, t)
        return radianReset(yaw),
               clamp(pitch, math.pi / 2),
               math.max(0.01, t)
    end

    -- 求解器
    local result = Optimizers.levenbergMarquardt(
        loss, projector, 0, maxIterations, yaw_0, pitch_0, t_0, tol, lambda, h
    )

    -- 求解失败，返回初始值
    if result.status ~= "VALID" then
        return { yaw = yaw_0, pitch = pitch_0, value = t_0, loss = result.loss, status = result.status }
    end
    
    return { yaw = result.x, pitch = result.y, value = result.z, loss = result.loss, status = "VALID" }
end

```

在构建 `loss` 函数时，我们需要构造炮弹在世界空间的位置函数，并补偿发射平台（船体）自身的运动。

-   **位置偏移**：考虑炮口相对船体质心的偏移 `mount_offset` ($o_s$) 以及炮管长度 `barrel_length` ($l_b$)。
-   **初速度叠加**：炮弹射出的世界初速度 `v_muzzle_world` ($\pmb{v}_{w,b,0}$) 由炮弹初速 `muzzle_vel` ($v_b$)、船体平移速度 `shooter.vel` ($v_s$)，以及旋转产生的切向速度 `v_tangential` 共同决定。
-   **炮弹位置**：使用解析位移公式（前文 trajectory 函数）
-   **其他**： `shooter.rotation` 即为世界坐标系到炮体坐标系的四元数 $q_{s2w}$，`shooter.pos` ($x_{w,s}$)即为世界坐标系下的船质心位置。
```lua
-- 基于 ballistic.lua 中的 loss 函数逻辑：
local dir_world = shooter.rotation:transform(dir_local)
local r_muzzle = shooter.rotation:transform(mount_offset):add(dir_world:mul(barrel_length))
local p_muzzle = shooter.pos:add(r_muzzle)

-- 运动学补偿：叠加角速度产生的切向速度
local v_tangential = shooter.omega:cross(r_muzzle)
local v_muzzle_world = shooter.vel:add(v_tangential):add(dir_world:mul(muzzle_vel))

-- 炮弹位置：使用解析位移公式（前文 trajectory 函数）
local trajectoryFunc = function (t)
    return p_muzzle:add(trajectory(v_muzzle_world, t, c, g))
end
```

### 3. 启发式初始化 (`aimPredictLM`)

由于 LM 算法是局部优化方法，极易陷入错误的局部最优解。在调用 LM 之前，我们先执行 5 次简单的线性搜索来初始化 $[t, \theta, \phi]$：

```lua
-- 简单几何搜索：迭代 5 次以寻找初始碰撞点
-- distance即简单的认为是目标与自身质心的初始距离
local t_est = distance / muzzle_vel
for _ = 1, 5 do
    local p_future = targetFunc(t_est)
    t_est = p_future:distance(p_muzzle) / muzzle_vel
end

-- 将得到的 p_future 转换为对应的偏航角和仰角，作为算法起点
local yaw_0, pitch_0 = calculateRotation(p_future - p_muzzle)
```

### 4. LM 算法数值求解流程

最后，进入 `levenbergMarquardt` 核心循环。该算法通过将高斯-牛顿（快速收敛）与梯度下降（稳健性）相结合，在每一步迭代中执行以下逻辑：

```lua
-- func: 残差函数，输入为 [yaw, pitch, t]，输出为 [x, y, z]，在我们的使用中，即为炮弹位置与目标位置的差值
-- projector: 投影函数，输入为 [yaw, pitch, t]，输出为 [yaw, pitch, t]，用于保证解在可行域内
-- f_tol: 残差平方和的容忍度，当残差平方和小于该值时，认为解已收敛
-- maxIter: 最大迭代次数
-- x0, y0, z0: 初始解
-- tol: 梯度范数的容忍度，当梯度范数小于该值时，认为解已收敛
-- lambda: 正则化参数，用于控制正则化强度
-- h: 有限差分步长
function levenbergMarquardt(func, projector, f_tol, maxIter, x0, y0, z0, tol, lambda, h)
    local x, y, z = x0, y0, z0
    local r = func(x, y, z)
    local m = #r
    local loss = 0
    for _, v in ipairs(r) do loss = loss + v * v end

    local currentLambda = lambda
    local JtJ = Matrix3d:new()
    local Jtr = {0, 0, 0}

    for iter = 1, maxIter do
        if loss < f_tol then
            return {x = x, y = y, z = z, loss = loss, status = "VALID"}
        end

        local r_xp = func(x + h, y, z)
        local r_xm = func(x - h, y, z)
        local r_yp = func(x, y + h, z)
        local r_ym = func(x, y - h, z)
        local r_zp = func(x, y, z + h)
        local r_zm = func(x, y, z - h)

        if not r_xp or not r_xm or not r_yp or not r_ym or not r_zp or not r_zm then
            return {x = x, y = y, z = z, loss = loss, status = "INVALID"}
        end

        JtJ:zero()
        Jtr[1], Jtr[2], Jtr[3] = 0, 0, 0

        for k = 1, m do -- m=3 
            local j_xk = (r_xp[k] - r_xm[k]) / (2 * h)
            local j_yk = (r_yp[k] - r_ym[k]) / (2 * h)
            local j_zk = (r_zp[k] - r_zm[k]) / (2 * h)
            local val = r[k]

            JtJ.m00 = JtJ.m00 + j_xk * j_xk
            JtJ.m01 = JtJ.m01 + j_xk * j_yk
            JtJ.m02 = JtJ.m02 + j_xk * j_zk

            JtJ.m11 = JtJ.m11 + j_yk * j_yk
            JtJ.m12 = JtJ.m12 + j_yk * j_zk

            JtJ.m22 = JtJ.m22 + j_zk * j_zk

            Jtr[1] = Jtr[1] + j_xk * val
            Jtr[2] = Jtr[2] + j_yk * val
            Jtr[3] = Jtr[3] + j_zk * val
        end
        -- Symmetry
        JtJ.m10 = JtJ.m01
        JtJ.m20 = JtJ.m02
        JtJ.m21 = JtJ.m12

        local improved = false
        for k = 1, 10 do
            local H_damp = JtJ:copy()
            H_damp.m00 = H_damp.m00 + currentLambda
            H_damp.m11 = H_damp.m11 + currentLambda
            H_damp.m22 = H_damp.m22 + currentLambda

            local det = H_damp:determinant()
            if math.abs(det) < 1e-12 then
                currentLambda = currentLambda * 10
            else
                local H_inv = H_damp:invert()
                local dx = -(H_inv.m00 * Jtr[1] + H_inv.m01 * Jtr[2] + H_inv.m02 * Jtr[3])
                local dy = -(H_inv.m10 * Jtr[1] + H_inv.m11 * Jtr[2] + H_inv.m12 * Jtr[3])
                local dz = -(H_inv.m20 * Jtr[1] + H_inv.m21 * Jtr[2] + H_inv.m22 * Jtr[3])

                local px, py, pz = projector(x + dx, y + dy, z + dz)
                
                local r_new = func(px, py, pz)
                local loss_new = 0
                for _, v in ipairs(r_new) do loss_new = loss_new + v * v end

                if loss_new < loss then
                    x, y, z = px, py, pz
                    loss = loss_new
                    r = r_new
                    currentLambda = math.max(1e-7, currentLambda / 10.0)
                    improved = true
                    break
                else
                    currentLambda = currentLambda * 10.0
                end
            end
        end

        if not improved then
             return {x = x, y = y, z = z, loss = loss, status = "VALID"}
        end
    end
    return {x = x, y = y, z = z, loss = loss, status = "VALID"}
end

```

#### (1) 有限差分雅可比矩阵估计
由于弹道方程和旋转补偿的复杂性，我们无法通过直接求导获取解析雅可比。因此，算法对状态变量 $[\theta, \phi, t]$ 中的每一个分量分别施加微小扰动 $h$，测量残差 $\pmb{f} = \pmb{x}_b - \pmb{x}_t$ 的变化：

```lua
-- 对变量施加扰动，分别测量残差变化
local r_xp = func(x + h, y, z)
local r_xm = func(x - h, y, z)
-- ...（同理处理 y 和 z 分置分量）...

-- 计算雅可比矩阵分量 (中心差分)
local j_xk = (r_xp[k] - r_xm[k]) / (2 * h)
```
最终构造出一个 $3 \times 3$ 的雅可比矩阵 $\pmb{J}$，其中 $\pmb{J}_{ik}$ 表示第 $i$ 个位移分量对第 $k$ 个优化参数的导数。

#### (2) 构造并求解正规方程
我们需要解出步进矢量 $\pmb{h}$。首先构造 Hessian 矩阵的近似项 $\pmb{J}^T \pmb{J}$ 和梯度方向矢量 $\pmb{J}^T \pmb{f}$。为了确保矩阵的正定性并处理非线性，引入阻尼系数 $\lambda$：

```lua
local H_damp = JtJ:copy()
H_damp.m00 = H_damp.m00 + currentLambda
H_damp.m11 = H_damp.m11 + currentLambda
H_damp.m22 = H_damp.m22 + currentLambda

-- 求解关键方程：(J^T * J + λ * I) * h = -J^T * f
local h = H_damp:invert():mul(-Jtr)
```
- **$\lambda$ 的调节作用**：当 $\lambda \to 0$ 时，模型趋向于通过抛物面近似的高斯-牛顿法，收敛极快；当 $\lambda \to \infty$ 时，步长变短且方向趋向梯度下降，确保了即便初始值较差也能稳定走向解。

#### (3) 阻尼更新与“试错”逻辑 (Marquardt Policy)
这是算法最核心的内循环（也称为 Marquardt 策略）。针对求出的步进 $\pmb{h}$，算法会进行尝试：

1.  **参数投影**：计算新状态 $\pmb{p}_{new} = \pmb{p} + \pmb{h}$，并通过 `projector` 函数强制执行物理约束（如 $\phi \in [-\pi/2, \pi/2]$，飞行时间 $t > 0$）。
2.  **误差验证**：计算新状态下的总误差 $L_{new}$。
    -   **若误差减小 ($L_{new} < L$)**：接受更新，令 $\lambda = \lambda / 10$。这表示模型线性度较好，尝试在下一步加速收敛。
    -   **若误差增加 ($L_{new} \ge L$)**：拒绝更新，令 $\lambda = \lambda \cdot 10$。这表明当前区域非线性太强，退回更稳健的梯度下降模式重新搜索。

#### (4) 终止条件
算法会循环迭代直至满足：
-   **残差收敛**：位移误差小于 `tol`。
-   **迭代耗尽**：达到 `maxIter`（通常 20 次以内即可收敛）。

通过这套逻辑，即便在极端的运动状态下，系统也能在数毫秒内解算出厘米级的拦截点，实现真正的“手术刀”式精准打击。
