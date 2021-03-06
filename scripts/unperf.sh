# https://easyperf.net/blog/2019/08/02/Perf-measurement-environment-on-Linux

set -e

echoto() {
	echo "echo $1 > $2"
	echo "$1" > "$2"
}

# 1. Disable turbo boost

if [ -f /sys/devices/system/cpu/intel_pstate/no_turbo ]; then
  # Intel
  echoto 0 /sys/devices/system/cpu/intel_pstate/no_turbo
elif [ -f /sys/devices/system/cpu/cpufreq/boost ]; then
  # AMD
  echoto 1 /sys/devices/system/cpu/cpufreq/boost
fi

# 2. Disable Hyper-Threading
for ONLINE in /sys/devices/system/cpu/cpu*/online; do
	echoto 1 $ONLINE
done

# 3. Set scaling_governor to ‘performance’
for GOV in /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
do
  echoto powersave $GOV || true
done

# 4. Set cpu affinity
# 5. Set process priority
# 6. Drop file system cache
# 7. Disable address space randomization
# 8. Use statistical methods to process measurements (we use criterion)
