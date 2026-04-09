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
#ifndef ORES_QT_ANALYTICS_PLUGIN_HPP
#define ORES_QT_ANALYTICS_PLUGIN_HPP

#include <memory>
#include <QList>
#include "ores.qt/PluginBase.hpp"

namespace ores::qt {

class PricingEngineTypeController;
class PricingModelConfigController;
class PricingModelProductController;
class PricingModelProductParameterController;

/**
 * @brief Plugin owning all analytics and pricing model controllers.
 *
 * Provides a top-level Analytics menu with:
 *   - Pricing Models submenu (pricing engine types)
 *   - Configuration submenu (model configs, products, parameters)
 */
class AnalyticsPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit AnalyticsPlugin(QObject* parent = nullptr);
    ~AnalyticsPlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.analytics"); }
    int load_order() const override { return 350; }

    void on_login(const plugin_context& ctx) override;
    QList<QMenu*> create_menus() override;
    void on_logout() override;

private:
    plugin_context ctx_;

    std::unique_ptr<PricingEngineTypeController>            pricingEngineTypeController_;
    std::unique_ptr<PricingModelConfigController>           pricingModelConfigController_;
    std::unique_ptr<PricingModelProductController>          pricingModelProductController_;
    std::unique_ptr<PricingModelProductParameterController> pricingModelProductParameterController_;
};

}

#endif
