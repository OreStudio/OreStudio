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
#include "ores.qt/export.hpp"
#include "ores.qt/plugin_context.hpp"

class QMenu;
class QAction;

namespace ores::qt {

/**
 * @brief Abstract interface that every domain plugin must implement.
 *
 * The host (MainWindow) calls these methods at well-defined lifecycle points:
 *
 *  1. on_login() — called after authentication succeeds; create controllers here.
 *  2. create_menus() — called after on_login(); return domain menus to be inserted
 *     into the menu bar.  The host takes ownership of the returned QMenu objects.
 *  3. on_logout() — called when the client disconnects; destroy controllers here.
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
     * @brief Build and return the domain menus contributed by this plugin.
     *
     * Called by the host after on_login().  The host inserts the returned menus
     * into the menu bar before the System menu and deletes them on logout.
     *
     * The returned QMenu objects should be created with nullptr parent; the host
     * will re-parent them to the menu bar on insertion.
     */
    virtual QList<QMenu*> create_menus() = 0;

    /**
     * @brief Destroy controllers and unsubscribe from server events.
     *
     * Called when the client disconnects, before the returned menus are removed.
     */
    virtual void on_logout() = 0;
};

}

#endif
