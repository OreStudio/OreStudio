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
#ifndef ORES_QT_IPLUGIN_HPP
#define ORES_QT_IPLUGIN_HPP

#include <QList>
#include <QString>
#include <QtPlugin>
#include "ores.qt/export.hpp"
#include "ores.qt/plugin_context.hpp"

class QMenu;
class QAction;

namespace ores::qt {

/**
 * @brief Context passed to setup_menus() containing all host-owned shared menus.
 *
 * Plugins may freely add sub-menus or actions to any of these menus.
 * Pointers may be null if the host has not created that menu yet.
 */
struct shared_menus_context {
    QMenu* system_menu = nullptr;
    QMenu* reference_data_menu = nullptr;
    QMenu* telemetry_menu = nullptr;
    QMenu* identity_menu = nullptr;
    QMenu* data_transfer_menu = nullptr;
    QMenu* trading_codes_menu = nullptr;
};

/**
 * @brief Abstract interface that every domain plugin must implement.
 *
 * The host (MainWindow) calls these methods at well-defined lifecycle points:
 *
 *  Startup (once, before any login):
 *    1. setup_menus(ctx)  — contribute items to host-owned shared menus.
 *    2. create_menus()    — return standalone domain menus; host inserts them
 *                           into the menu bar (disabled until login).
 *    3. toolbar_actions() — return actions to add to the main toolbar.
 *
 *  Per-session:
 *    4. on_login(ctx)  — authentication succeeded; create controllers here.
 *    5. on_logout()    — client disconnected; destroy controllers here.
 *
 * Menus and toolbar actions are created once at startup and live for the entire
 * application lifetime.  on_login()/on_logout() enable/disable them without
 * recreating them.
 *
 * Implementations must inherit from QObject as well as IPlugin so that signal/slot
 * connections can be made between plugin internals and the host.
 */
class ORES_QT_API IPlugin {
public:
    virtual ~IPlugin() = default;

    /**
     * @brief Returns a short, unique plugin name (e.g. "ores.qt.legacy").
     */
    virtual QString name() const = 0;

    /**
     * @brief Load order hint — lower values are initialised first (default 100).
     */
    virtual int load_order() const { return 100; }

    /**
     * @brief Create controllers and subscribe to server events.
     *
     * Called immediately after successful login.  @p ctx provides the shared
     * application resources (client, caches, MDI area, etc.).
     */
    virtual void on_login(const plugin_context& ctx) = 0;

    /**
     * @brief Contribute items to host-owned shared menus.
     *
     * Called once at startup, before create_menus().  The host passes a
     * struct containing pre-created menus that plugins may freely add
     * sub-menus or actions to.
     *
     * Plugins that only need to add sub-menus to these shared containers
     * should do all their work here and return {} from create_menus().
     *
     * The default implementation does nothing.
     */
    virtual void setup_menus(const shared_menus_context& ctx) {
        Q_UNUSED(ctx)
    }

    /**
     * @brief Build and return standalone domain menus for this plugin.
     *
     * Called once at startup, after setup_menus().  The host inserts the
     * returned menus into the menu bar before the &System menu, in plugin
     * load_order.  Menus are initially disabled and are enabled on login.
     *
     * Plugins that contribute exclusively to shared menus via setup_menus()
     * should return an empty list here.
     */
    virtual QList<QMenu*> create_menus() = 0;

    /**
     * @brief Destroy controllers and unsubscribe from server events.
     *
     * Called when the client disconnects, before the returned menus are removed.
     */
    virtual void on_logout() = 0;

    /**
     * @brief Return actions to be added to the main toolbar.
     *
     * Called once at startup (after create_menus()).  The returned QAction
     * objects must be the same instances that appear in the plugin's menus so
     * that enable/disable state is shared automatically.  The host inserts a
     * separator between each plugin's toolbar group.
     *
     * The default implementation returns an empty list (no toolbar contribution).
     */
    virtual QList<QAction*> toolbar_actions() { return {}; }
};

}

Q_DECLARE_INTERFACE(ores::qt::IPlugin, "ores.qt.IPlugin/1.0")

#endif
