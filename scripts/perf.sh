# https://easyperf.net/blog/2019/08/02/Perf-measurement-environment-on-Linux

set -ex

# 1. Disable turbo boost
echo 1 > /sys/devices/system/cpu/intel_pstate/no_turbo

# 2. Disable Hyper-Threading
echo 0 > /sys/devices/system/cpu/cpu2/online
echo 0 > /sys/devices/system/cpu/cpu3/online

# 3. Set scaling_governor to ‘performance’
for i in 0 1 2 3
do
  echo performance > /sys/devices/system/cpu/cpu$i/cpufreq/scaling_governor
done

# 4. Set cpu affinity
# 5. Set process priority
# 6. Drop file system cache
# 7. Disable address space randomization
# 8. Use statistical methods to process measurements (we use criterion)
