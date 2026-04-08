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
#ifndef ORES_QT_PLUGIN_REGISTRY_HPP
#define ORES_QT_PLUGIN_REGISTRY_HPP

#include <QString>
#include <QVector>
#include "ores.qt/export.hpp"
#include "ores.qt/IPlugin.hpp"

class QPluginLoader;

namespace ores::qt {

/**
 * @brief Singleton registry that discovers and owns all loaded domain plugins.
 *
 * Call load_from_directory() once at application startup (before constructing
 * MainWindow) to scan a directory for plugin shared libraries.  Each .so/.dll
 * that implements IPlugin is loaded, sorted by IPlugin::load_order(), and made
 * available via plugins().
 *
 * The registry retains QPluginLoader instances for the lifetime of the process
 * so that plugin .so files are never unmapped while controllers are live.
 */
class ORES_QT_API PluginRegistry {
public:
    /**
     * @brief Returns the application-wide registry instance.
     *
     * Must be called only after initialise() has been called from main().
     * The registry lives on the stack in main() so it is destroyed before
     * static atexit handlers (including Boost.Log teardown), which ensures
     * that BOOST_LOG calls in plugin and controller destructors are safe.
     */
    static PluginRegistry& instance();

    /**
     * @brief Registers the stack-owned registry instance.
     *
     * Call once from main() immediately after constructing the PluginRegistry
     * object and before any other code that calls instance().
     */
    static void initialise(PluginRegistry& registry);

    /**
     * @brief Scan @p plugin_dir for shared libraries, load each IPlugin found,
     *        and sort the results by load_order() ascending.
     *
     * Unloadable files and files that do not implement IPlugin are skipped with
     * a warning logged via qWarning().  Safe to call only once.
     */
    void load_from_directory(const QString& plugin_dir);

    /**
     * @brief Return the ordered list of successfully loaded plugins.
     *
     * Plugins are ordered by load_order() ascending (admin first, trading last).
     */
    const QVector<IPlugin*>& plugins() const { return plugins_; }

    PluginRegistry() = default;
    ~PluginRegistry();
    PluginRegistry(const PluginRegistry&) = delete;
    PluginRegistry& operator=(const PluginRegistry&) = delete;

private:

    QVector<QPluginLoader*> loaders_;  ///< Must outlive plugin instances.
    QVector<IPlugin*>       plugins_;
};

}

#endif
