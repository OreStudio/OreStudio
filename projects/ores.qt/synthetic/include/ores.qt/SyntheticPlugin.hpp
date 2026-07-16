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
#ifndef ORES_QT_SYNTHETIC_PLUGIN_HPP
#define ORES_QT_SYNTHETIC_PLUGIN_HPP

#include "ores.qt/PluginBase.hpp"
#include <QList>
#include <memory>

class QAction;
class QMenu;
class QMdiSubWindow;

namespace ores::qt {

class MarketDataGenerationConfigController;
class FxSpotGenerationConfigController;
class GmmComponentController;
class IrCurveGenerationConfigController;
class IrCurveTemplateEntryController;
class YieldCurveProcessTypeController;

/**
 * @brief Synthetic plugin: synthetic market data generation configuration.
 *
 * Loaded as a shared library by QPluginLoader at application startup.
 * Contributes entries to the shared Market Data menu owned by MktdataPlugin.
 */
class SyntheticPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit SyntheticPlugin(QObject* parent = nullptr);
    ~SyntheticPlugin() override;

    QString name() const override {
        return QStringLiteral("ores.qt.synthetic");
    }
    int load_order() const override {
        return 310;
    }

    void on_login(const plugin_context& ctx) override;
    void setup_menus(const shared_menus_context& ctx) override;
    QList<QMenu*> create_menus() override;
    QList<QAction*> toolbar_actions() override;
    void on_logout() override;

private:
    plugin_context ctx_;

    QMenu* marketDataMenu_ = nullptr;
    QMdiSubWindow* marketSimulatorWindow_ = nullptr;
    std::unique_ptr<MarketDataGenerationConfigController> configController_;
    std::unique_ptr<FxSpotGenerationConfigController> fxSpotConfigController_;
    std::unique_ptr<GmmComponentController> gmmComponentController_;
    std::unique_ptr<IrCurveGenerationConfigController> irCurveGenerationConfigController_;
    std::unique_ptr<IrCurveTemplateEntryController> irCurveTemplateEntryController_;
    std::unique_ptr<YieldCurveProcessTypeController> yieldCurveProcessTypeController_;
};

}

#endif
