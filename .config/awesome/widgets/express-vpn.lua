local awful = require("awful")
local wibox = require("wibox")
local watch = require("awful.widget.watch")

local GET_EXPRESS_STATUS = 'bash -c "expressvpn status"'
local PATH_TO_ICONS = '/usr/share/icons/Arc'

local express_widget = wibox.widget {
    {
        id = "icon",
        widget = wibox.widget.imagebox,
        image = PATH_TO_ICONS .. "/status/symbolic/network-vpn-symbolic.svg"
    },
    layout = wibox.layout.align.horizontal,
    set_status = function(self, disconnected)
        if disconnected then
            self.icon.image = PATH_TO_ICONS .. "/status/symbolic/network-vpn-acquiring-symbolic.svg"
        else
            self.icon.image = PATH_TO_ICONS .. "/status/symbolic/network-vpn-symbolic.svg"
        end
    end,
}

return watch(
    GET_EXPRESS_STATUS,
    1,
    function(widget, stdout)
        disconnected = string.find(stdout, 'Not connected') ~= nil

        widget:set_status(disconnected)
    end,
    express_widget
)
