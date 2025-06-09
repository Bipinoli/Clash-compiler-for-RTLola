FROM hdlc/ghdl:yosys

RUN apt update && apt install -y busybox

ENTRYPOINT [ "python3", "/thesis/extract_stats.py", "/thesis/specs"]