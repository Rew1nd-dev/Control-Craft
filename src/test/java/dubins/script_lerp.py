import matplotlib.pyplot as plt
import numpy as np
import sys
import re

def plot_dubins(filename):
    # 读取数据
    points = []
    start = None
    goal = None
    path_type = "Unknown"
    total_length = 0.0
    
    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                if "Start:" in line:
                    start = parse_vector(line.split(":")[1].strip())
                elif "Goal:" in line:
                    goal = parse_vector(line.split(":")[1].strip())
                elif "Type:" in line:
                    path_type = line.split(":")[1].strip()
                elif "Total length:" in line:
                    total_length = float(line.split(":")[1].strip())
                continue
            x, y = map(float, line.split(','))
            points.append((x, y))
    
    if not points:
        print(f"No points found in {filename}")
        return
    
    # 转换为numpy数组
    points = np.array(points)
    
    # 创建图形
    plt.figure(figsize=(10, 8))
    plt.title(f"Dubins Path: {path_type}\nLength: {total_length:.2f}")
    
    # 绘制路径
    plt.plot(points[:, 0], points[:, 1], 'b-', linewidth=1.5, label='Path')
    plt.plot(points[:, 0], points[:, 1], 'ro', markersize=2, alpha=0.5)
    
    # 标记起点和终点
    if start:
        plt.plot(start[0], start[1], 'go', markersize=8, label='Start')
        plt.quiver(start[0], start[1], 
                   np.cos(0), np.sin(0), 
                   color='g', scale=10, width=0.005)
    
    if goal:
        plt.plot(goal[0], goal[1], 'mo', markersize=8, label='Goal')
        plt.quiver(goal[0], goal[1], 
                   np.cos(np.pi/2), np.sin(np.pi/2), 
                   color='m', scale=10, width=0.005)
    
    # 添加图例和网格
    plt.legend()
    plt.grid(True)
    plt.axis('equal')
    
    # 保存图像
    # output_file = filename.replace('.txt', '.png')
    # plt.savefig(output_file)
    # print(f"Saved plot to {output_file}")
    
    # 显示图像
    plt.show()

def parse_vector(vec_str):
    """解析JOML Vector2d格式的字符串，如'(0.0 0.0)'或'Vector2d(0.0, 0.0)'"""
    # 尝试匹配科学计数法或常规浮点数
    pattern = r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?"
    coords = re.findall(pattern, vec_str)
    
    if len(coords) >= 2:
        try:
            x = float(coords[0])
            y = float(coords[1])
            return (x, y)
        except ValueError:
            pass
    
    # 尝试匹配带逗号的格式
    if ',' in vec_str:
        parts = vec_str.split(',')
        if len(parts) >= 2:
            try:
                x = float(parts[0].strip())
                y = float(parts[1].strip())
                return (x, y)
            except ValueError:
                pass
    
    # 如果所有方法都失败，打印错误
    print(f"Warning: Could not parse vector string: {vec_str}")
    return (0, 0)

if __name__ == "__main__":
    plot_dubins('case2.txt')
        