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
#ifndef ORES_QT_WORKSPACE_PLUGIN_HPP
#define ORES_QT_WORKSPACE_PLUGIN_HPP

#include "ores.qt/PluginBase.hpp"
#include <QList>
#include <memory>

class QAction;

namespace ores::qt {

class WorkspaceController;

/**
 * @brief Workspace plugin: manage named data-isolation workspaces.
 *
 * Contributes "Manage Workspaces" to the shared Data Management menu;
 * no standalone top-level menu is created.
 */
class WorkspacePlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit WorkspacePlugin(QObject* parent = nullptr);
    ~WorkspacePlugin() override;

    QString name() const override {
        return QStringLiteral("ores.qt.workspace");
    }
    int load_order() const override {
        return 210;
    } // after TradingPlugin (200)

    void on_login(const plugin_context& ctx) override;
    void setup_menus(const shared_menus_context& ctx) override;
    QList<QMenu*> create_menus() override;
    void on_logout() override;

private:
    plugin_context ctx_;
    QAction* act_workspaces_{nullptr};
    std::unique_ptr<WorkspaceController> workspaceController_;
};

}

#endif
