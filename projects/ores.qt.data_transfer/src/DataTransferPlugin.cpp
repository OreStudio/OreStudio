/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/DataTransferPlugin.hpp"

#include <QMenu>
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.qt.data_transfer_plugin");
    return instance;
}

}

DataTransferPlugin::DataTransferPlugin(QObject* parent) : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

DataTransferPlugin::~DataTransferPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void DataTransferPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;
}

void DataTransferPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Capturing shared Data Transfer menu handle."
        << " data_transfer=" << (smc.data_transfer_menu ? "ok" : "null");
    // Save reference so create_menus() can return the pre-created menu.
    // RefdataPlugin and TradingPlugin contribute actions during their own
    // setup_menus calls.
    data_transfer_menu_ = smc.data_transfer_menu;
}

QList<QMenu*> DataTransferPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "Building plugin menus."
        << " data_transfer_menu=" << (data_transfer_menu_ ? "ok" : "null");
    if (!data_transfer_menu_) {
        BOOST_LOG_SEV(lg(), warn) << "Data Transfer menu handle is missing — no menu will appear.";
        return {};
    }
    return {data_transfer_menu_};
}

void DataTransferPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    ctx_ = {};
}

} // namespace ores::qt
