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
#include "ores.qt/PluginRegistry.hpp"
#include <algorithm>
#include <QDir>
#include <QLibrary>
#include <QPluginLoader>

namespace ores::qt {

PluginRegistry& PluginRegistry::instance() {
    static PluginRegistry registry;
    return registry;
}

PluginRegistry::~PluginRegistry() {
    plugins_.clear();
    for (auto* loader : loaders_) {
        loader->unload();
        delete loader;
    }
}

void PluginRegistry::load_from_directory(const QString& plugin_dir) {
    QDir dir(plugin_dir);
    if (!dir.exists()) {
        qWarning() << "PluginRegistry: plugin directory does not exist:"
                   << plugin_dir;
        return;
    }

    QVector<QPair<int, IPlugin*>> ordered;

    for (const QString& filename : dir.entryList(QDir::Files)) {
        const QString filepath = dir.absoluteFilePath(filename);
        if (!QLibrary::isLibrary(filepath))
            continue;

        auto* loader = new QPluginLoader(filepath);
        QObject* obj = loader->instance();
        if (!obj) {
            qWarning() << "PluginRegistry: failed to load" << filename
                       << "-" << loader->errorString();
            delete loader;
            continue;
        }

        auto* plugin = qobject_cast<IPlugin*>(obj);
        if (!plugin) {
            qWarning() << "PluginRegistry:" << filename
                       << "does not implement IPlugin";
            loader->unload();
            delete loader;
            continue;
        }

        loaders_.push_back(loader);
        ordered.push_back({plugin->load_order(), plugin});
    }

    std::stable_sort(ordered.begin(), ordered.end(),
        [](const QPair<int, IPlugin*>& a, const QPair<int, IPlugin*>& b) {
            return a.first < b.first;
        });

    for (const auto& pair : ordered)
        plugins_.push_back(pair.second);
}

}
