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
#ifndef ORES_QT_MKTDATA_PLUGIN_HPP
#define ORES_QT_MKTDATA_PLUGIN_HPP

#include <memory>
#include <QList>
#include "ores.qt/PluginBase.hpp"

class QAction;

namespace ores::qt {

class CurrencyMarketTierController;
class MarketDataController;

/**
 * @brief Market data plugin: market series, fixings, and currency market tiers.
 *
 * Loaded as a shared library by QPluginLoader at application startup.
 * Owns the Market Data menu.
 */
class MktdataPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit MktdataPlugin(QObject* parent = nullptr);
    ~MktdataPlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.mktdata"); }
    int load_order() const override { return 300; }

    void on_login(const plugin_context& ctx) override;
    QList<QMenu*> create_menus() override;
    QList<QAction*> toolbar_actions() override;
    void on_logout() override;

private:

    plugin_context ctx_;

    // Entity controllers
    std::unique_ptr<CurrencyMarketTierController>  currencyMarketTierController_;
    std::unique_ptr<MarketDataController>          marketDataController_;
};

}

#endif
