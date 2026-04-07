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

#include <memory>
#include <vector>
#include "ores.qt/export.hpp"
#include "ores.qt/IPlugin.hpp"

namespace ores::qt {

/**
 * @brief Singleton registry that owns all loaded plugins.
 *
 * Plugins are registered at application startup (before MainWindow is shown)
 * via register_plugin().  MainWindow iterates plugins() on login/logout.
 */
class ORES_QT_API PluginRegistry {
public:
    static PluginRegistry& instance();

    /**
     * @brief Register a plugin.  Plugins are stored in registration order.
     *
     * Ownership is transferred to the registry.
     */
    void register_plugin(std::unique_ptr<IPlugin> plugin);

    /**
     * @brief Access the ordered list of registered plugins.
     */
    const std::vector<std::unique_ptr<IPlugin>>& plugins() const { return plugins_; }

private:
    PluginRegistry() = default;
    PluginRegistry(const PluginRegistry&) = delete;
    PluginRegistry& operator=(const PluginRegistry&) = delete;

    std::vector<std::unique_ptr<IPlugin>> plugins_;
};

}

#endif
