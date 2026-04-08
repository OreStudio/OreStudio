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
#ifndef ORES_QT_TRADING_PLUGIN_HPP
#define ORES_QT_TRADING_PLUGIN_HPP

#include <memory>
#include <QList>
#include "ores.qt/PluginBase.hpp"

class QAction;

namespace ores::qt {

class DetachableMdiSubWindow;
class OreImportController;
class PortfolioController;
class BookController;
class BookStatusController;
class TradeController;

/**
 * @brief Trading plugin: portfolios, books, trades, and the portfolio/org
 *        explorers.
 *
 * Extracted from LegacyPlugin in Step 8 of the Qt plugin refactor.
 * Owns the Data (portfolios/books) and Trading menus.
 */
/**
 * @brief Trading plugin: portfolios, books, trades, and the portfolio/org
 *        explorers.
 *
 * Loaded as a shared library by QPluginLoader at application startup.
 * Owns the Data (portfolios/books) and Trading menus.
 */
class TradingPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit TradingPlugin(QObject* parent = nullptr);
    ~TradingPlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.trading"); }
    int load_order() const override { return 200; }

    void on_login(const plugin_context& ctx) override;
    QList<QMenu*> create_menus() override;
    QList<QAction*> toolbar_actions() override;
    void on_logout() override;

private:

    plugin_context ctx_;

    // Singleton MDI sub-windows (nullptr when not open)
    DetachableMdiSubWindow* portfolio_explorer_sub_window_{nullptr};
    DetachableMdiSubWindow* org_explorer_sub_window_{nullptr};

    QAction* act_portfolios_{nullptr};
    QAction* act_books_{nullptr};
    QAction* act_portfolio_explorer_{nullptr};
    QAction* act_org_explorer_{nullptr};
    QAction* act_trades_{nullptr};
    QAction* act_import_ore_{nullptr};

    // Entity controllers
    std::unique_ptr<OreImportController>   oreImportController_;
    std::unique_ptr<PortfolioController>   portfolioController_;
    std::unique_ptr<BookController>        bookController_;
    std::unique_ptr<BookStatusController>  bookStatusController_;
    std::unique_ptr<TradeController>       tradeController_;
};

}

#endif
