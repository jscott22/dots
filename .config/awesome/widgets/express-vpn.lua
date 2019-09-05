local awful = require("awful")
local wibox = require("wibox")
local naughty = require("naughty")
local watch = require("awful.widget.watch")

local GET_EXPRESS_STATUS_ICON = [[ bash -c "
 expressvpn status | head -n 1
"]]

local GET_EXPRESS_STATUS_TEXT = [[ bash -c "
 expressvpn status | head -n 1 | sed 's/^.*\[.*m//'
"]]

local PATH_TO_ICONS = '/usr/share/icons/Arc'

local express_widget = wibox.widget {
    {
        id = "icon",
        widget = wibox.widget.imagebox,
        image = PATH_TO_ICONS .. "/status/symbolic/network-vpn-symbolic.svg"
    },
    layout = wibox.layout.align.horizontal,
    set_status = function(self, icon)
        self.icon.image = PATH_TO_ICONS .. icon 
    end,
}

local notification
local function show_vpn_status()
    awful.spawn.easy_async(GET_EXPRESS_STATUS_TEXT,
        function(stdout, _, _, _)
            naughty.destroy(notification)
            notification = naughty.notify{
                text =  stdout,
                title = "ExpressVPN Status",
                timeout = 5, hover_timeout = 0.5,
                width = 400,
            }
        end
    )
end

watch(
    GET_EXPRESS_STATUS_ICON,
    1,
    function(widget, stdout)
        local icon

        if string.find(stdout, 'Not connected') ~= nil
        then icon = "/status/symbolic/network-error-symbolic.svg"
        elseif string.find(stdout, 'Connecting') ~= nil
        then icon = "/status/symbolic/network-vpn-acquiring-symbolic.svg"
        else icon = "/status/symbolic/network-vpn-symbolic.svg"
        end

        widget:set_status(icon)
    end,
    express_widget
)

express_widget:connect_signal("mouse::enter", function() show_vpn_status() end)
express_widget:connect_signal("mouse::leave", function() naughty.destroy(notification) end)

return express_widget
