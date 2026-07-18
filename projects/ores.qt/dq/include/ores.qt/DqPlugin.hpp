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
#ifndef ORES_QT_DQ_PLUGIN_HPP
#define ORES_QT_DQ_PLUGIN_HPP

#include "ores.qt/PluginBase.hpp"
#include <QList>
#include <memory>

class QAction;
class QMenu;

namespace ores::qt {

class BadgeDefinitionController;
class BadgeSeverityController;
class CodeDomainController;

/**
 * @brief Qt plugin owning the ores.dq badge-governance entities' Qt
 * layer: badge_definition, badge_severity, code_domain (and, via
 * CodeDomainDetailDialog's BadgeMappingsTab, badge_mapping).
 *
 * Every entity this plugin owns is modeled in ores.dq -- the plugin
 * boundary lines up with the C++ component boundary, per the same
 * convention RefdataPlugin/AdminPlugin follow for their components.
 * Owns the pre-created data_quality_menu handle.
 */
class DqPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit DqPlugin(QObject* parent = nullptr);
    ~DqPlugin() override;

    QString name() const override {
        return QStringLiteral("ores.qt.dq");
    }
    int load_order() const override {
        return 380;
    }

    void on_login(const plugin_context& ctx) override;
    void setup_menus(const shared_menus_context& ctx) override;
    QList<QMenu*> create_menus() override;
    void on_logout() override;

private:
    plugin_context ctx_;

    // The data_quality_menu is pre-created by MainWindow and passed via
    // setup_menus context. We hold a reference to return it from create_menus.
    QMenu* data_quality_menu_{nullptr};

    std::unique_ptr<BadgeDefinitionController> badgeDefinitionController_;
    std::unique_ptr<BadgeSeverityController> badgeSeverityController_;
    std::unique_ptr<CodeDomainController> codeDomainController_;
};

}

#endif
