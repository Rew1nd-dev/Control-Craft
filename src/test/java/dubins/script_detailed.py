import matplotlib.pyplot as plt
import numpy as np
import re
import math

def plot_detailed_dubins(filename):
    # 读取路径信息
    start_pos = None
    start_heading = 0
    goal_pos = None
    goal_heading = 0
    path_type = "Unknown"
    total_length = 0
    turning_radius = 0
    segments = []
    current_segment = {}
    
    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
                
            if line.startswith('#'):
                # 处理元数据
                if "Start:" in line:
                    parts = re.findall(r"\(([^)]+)\)", line)
                    if parts:
                        coords = parts[0].split(',')
                        start_pos = (float(coords[0]), float(coords[1]))
                        heading_part = line.split("Heading:")[1]
                        start_heading = math.radians(float(re.search(r"[-+]?\d*\.?\d+", heading_part).group()))
                elif "Goal:" in line:
                    parts = re.findall(r"\(([^)]+)\)", line)
                    if parts:
                        coords = parts[0].split(',')
                        goal_pos = (float(coords[0]), float(coords[1]))
                        heading_part = line.split("Heading:")[1]
                        goal_heading = math.radians(float(re.search(r"[-+]?\d*\.?\d+", heading_part).group()))
                elif "Path Type:" in line:
                    path_type = line.split(":")[1].strip()
                elif "Total Length:" in line:
                    total_length = float(re.search(r"[-+]?\d*\.?\d+", line.split(":")[1]).group())
                elif "Turning Radius:" in line:
                    turning_radius = float(re.search(r"[-+]?\d*\.?\d+", line.split(":")[1]).group())
                continue
                
            # 处理段信息
            if line.startswith("Segment"):
                if current_segment:
                    segments.append(current_segment)
                parts = line.split(":")
                current_segment = {
                    "index": parts[0].split()[1],
                    "type": parts[1].strip()
                }
            elif line.startswith("Length:"):
                current_segment["length"] = float(line.split(":")[1].strip())
            elif line.startswith("Start:"):
                coords = re.findall(r"\(([^)]+)\)", line)
                if coords:
                    parts = coords[0].split(',')
                    current_segment["start"] = (float(parts[0]), float(parts[1]))
            elif line.startswith("End:"):
                coords = re.findall(r"\(([^)]+)\)", line)
                if coords:
                    parts = coords[0].split(',')
                    current_segment["end"] = (float(parts[0]), float(parts[1]))
            elif line.startswith("Center:"):
                coords = re.findall(r"\(([^)]+)\)", line)
                if coords:
                    parts = coords[0].split(',')
                    current_segment["center"] = (float(parts[0]), float(parts[1]))
            elif line.startswith("Radius:"):
                current_segment["radius"] = float(line.split(":")[1].strip())
            elif line.startswith("StartAngle:"):
                angle = float(re.search(r"[-+]?\d*\.?\d+", line).group())
                current_segment["start_angle"] = math.radians(angle)
            elif line.startswith("EndAngle:"):
                angle = float(re.search(r"[-+]?\d*\.?\d+", line).group())
                current_segment["end_angle"] = math.radians(angle)
                
        # 添加最后一个段
        if current_segment:
            segments.append(current_segment)
    
    # 创建图形
    plt.figure(figsize=(12, 10))
    ax = plt.gca()
    plt.title(f"Dubins Path: {path_type}\nTotal Length: {total_length:.2f}, Turning Radius: {turning_radius:.2f}")
    
    # 绘制起点和终点
    if start_pos:
        plt.plot(start_pos[0], start_pos[1], 'go', markersize=10, label='Start')
        plt.quiver(start_pos[0], start_pos[1], 
                   math.cos(start_heading), math.sin(start_heading),
                   color='g', scale=10, width=0.005)
    
    if goal_pos:
        plt.plot(goal_pos[0], goal_pos[1], 'mo', markersize=10, label='Goal')
        plt.quiver(goal_pos[0], goal_pos[1], 
                   math.cos(goal_heading), math.sin(goal_heading),
                   color='m', scale=10, width=0.005)
    
    # 绘制每个段
    colors = {'LEFT_TURN': 'red', 'RIGHT_TURN': 'blue', 'STRAIGHT': 'green'}
    
    for seg in segments:
        seg_type = seg["type"]
        start = seg["start"]
        end = seg["end"]
        
        # 绘制直线段
        if seg_type == "STRAIGHT":
            plt.plot([start[0], end[0]], [start[1], end[1]], 
                     color=colors[seg_type], linewidth=2, linestyle='-', 
                     label=f'Segment {seg["index"]}: Straight')
        
        # 绘制圆弧段
        else:
            center = seg["center"]
            radius = seg["radius"]
            start_angle = seg.get("start_angle", 0)
            end_angle = seg.get("end_angle", 0)
            
            # 确定圆弧方向
            if seg_type == "LEFT_TURN":  # 左转（逆时针）
                # 确保结束角度大于起始角度
                if end_angle < start_angle:
                    end_angle += 2 * math.pi
            else:  # 右转（顺时针）
                # 确保起始角度大于结束角度
                if start_angle < end_angle:
                    start_angle += 2 * math.pi
            
            # 生成圆弧点
            num_points = 100
            theta = np.linspace(start_angle, end_angle, num_points)
            x = center[0] + radius * np.cos(theta)
            y = center[1] + radius * np.sin(theta)
            
            plt.plot(x, y, color=colors[seg_type], linewidth=2, 
                     label=f'Segment {seg["index"]}: {seg_type}')
            
            # 绘制圆心
            plt.plot(center[0], center[1], 'k+', markersize=8)
    
    # 添加图例和网格
    handles, labels = plt.gca().get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    plt.legend(by_label.values(), by_label.keys(), loc='best')
    
    plt.grid(True)
    plt.axis('equal')
    
    # 保存图像
    output_file = filename.replace('.txt', '_detailed.png')
    plt.savefig(output_file, dpi=300)
    print(f"Saved detailed plot to {output_file}")
    
    # 显示图像
    plt.show()

if __name__ == "__main__":
    plot_detailed_dubins('detailed_case1.txt')
    
    