#!/bin/sh
#
# Script to simulate a low bandwith network by creating a transparent bridge
# on a machine with two NIC. This machine must be placed between the machines
# those traffic shall be limited.
#
# Warning: This builds a transparent bridge, do NOT plug both NIC into the same
# network. This would produce a loop.

set -e

rate1=512kbit
rate2=512kbit
delay1=100ms
delay2=100ms

#
# Start the throttling or change the throttling settings.
#
throttle()
{
	rate1=${1:-$rate1}
	rate2=${2:-$rate2}
	delay1=${3:-$delay1}
	delay2=${4:-$delay2}

	if ! brctl show | grep -q brSlow
	then
		brctl addbr brSlow
		brctl setfd brSlow 0
		brctl addif brSlow "$if1"
		brctl addif brSlow "$if2"
	fi

	ip link set up dev "$if1"
	ip link set up dev "$if2"
	ip link set up dev brSlow

	tc qdisc replace dev "$if1" root netem delay "$delay1" rate "$rate1"
	tc qdisc replace dev "$if2" root netem delay "$delay2" rate "$rate2"
}


#
# Stop the throttling, the bridge will still persist.
#
unthrottle()
{
	tc qdisc delete dev "$if1" root
	tc qdisc delete dev "$if2" root
}

#
# Stop the throttling and delete the bridge.
#
delete()
{
	unthrottle || true
	ip link set down dev brSlow
	ip link set down dev "$if1"
	ip link set down dev "$if2"
	brctl delbr brSlow
}


if [ -f /etc/slow-bridge.conf ]
then
	. /etc/slow-bridge.conf
else
	echo "Name the two interfaces you wish to bridge (space separated):"
	read if1 if2
	cat > /etc/slow-bridge.conf <<-END
		if1=$if1
		if2=$if2
		rate1=$rate1
		rate2=$rate2
		delay1=$delay1
		delay2=$delay2
		END
	echo "Configuration written to  /etc/slow-bridge.conf"
fi


if [ -z "$1" ]
then
	echo "usage:"
	echo "  $(basename $0) throttle [rate1 [rate2 [delay1 [delay2]]]]"
	echo "  $(basename $0) unthrottle"
	echo "  $(basename $0) delete"
	exit 1
fi
eval "$@"
