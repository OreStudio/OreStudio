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
#ifndef ORES_QT_PARTY_PLUGIN_HPP
#define ORES_QT_PARTY_PLUGIN_HPP

#include "ores.qt/PluginBase.hpp"
#include <QList>
#include <memory>

class QAction;

namespace ores::qt {

class PartyStatusController;
class PartyIdSchemeController;
class BusinessUnitController;
class BusinessUnitTypeController;

/**
 * @brief Party/organisation plugin: parties, counterparties, business
 *        units, and related type tables.
 *
 * Loaded as a shared library by QPluginLoader at application startup.
 * Contributes to the shared Reference Data menu; no standalone menu.
 */
class PartyPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit PartyPlugin(QObject* parent = nullptr);
    ~PartyPlugin() override;

    QString name() const override {
        return QStringLiteral("ores.qt.party");
    }
    int load_order() const override {
        return 105;
    } // after RefdataPlugin (100)

    void on_login(const plugin_context& ctx) override;
    void setup_menus(const shared_menus_context& ctx) override;
    QList<QMenu*> create_menus() override;
    QList<QAction*> toolbar_actions() override;
    void on_logout() override;

private:
    plugin_context ctx_;

    QAction* act_business_units_{nullptr};

    std::unique_ptr<PartyStatusController> partyStatusController_;
    std::unique_ptr<PartyIdSchemeController> partyIdSchemeController_;
    std::unique_ptr<BusinessUnitController> businessUnitController_;
    std::unique_ptr<BusinessUnitTypeController> businessUnitTypeController_;
};

}

#endif
