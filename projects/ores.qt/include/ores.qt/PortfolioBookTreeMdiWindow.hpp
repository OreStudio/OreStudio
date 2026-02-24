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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_PORTFOLIO_BOOK_TREE_MDI_WINDOW_HPP
#define ORES_QT_PORTFOLIO_BOOK_TREE_MDI_WINDOW_HPP

#include <vector>
#include <QLabel>
#include <QAction>
#include <QToolBar>
#include <QSplitter>
#include <QTreeView>
#include <QTableView>
#include <QDateTime>
#include <QFutureWatcher>
#include <QItemSelection>
#include <QSortFilterProxyModel>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/PaginationWidget.hpp"
#include "ores.qt/PortfolioBookTreeModel.hpp"
#include "ores.qt/PortfolioBookTradeModel.hpp"
#include "ores.refdata/domain/portfolio.hpp"
#include "ores.refdata/domain/book.hpp"

namespace ores::qt {

/**
 * @brief MDI window showing the portfolio/book hierarchy with a filtered trade table.
 *
 * Displays the full portfolio/book tree for the session party on the left.
 * Selecting any node scopes the trade table on the right to that subtree.
 * The stale indicator pulses when book, portfolio, or trade changes arrive.
 */
class PortfolioBookTreeMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.portfolio_book_tree_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    // Event names for subscription
    static constexpr const char* book_event = "ores.refdata.book_changed";
    static constexpr const char* portfolio_event = "ores.refdata.portfolio_changed";
    static constexpr const char* trade_event = "ores.trading.trade_changed";

public:
    explicit PortfolioBookTreeMdiWindow(
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);
    ~PortfolioBookTreeMdiWindow() override = default;

public slots:
    /**
     * @brief Reload all data from server.
     *
     * Fires parallel async fetches for portfolios, books, and counterparties.
     */
    void reload() override;

signals:
    void statusChanged(const QString& msg);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh portfolio/book tree");
    }

private slots:
    void onTreeSelectionChanged(const QItemSelection& selected,
                                const QItemSelection& deselected);
    void onNotificationReceived(const QString& eventType,
                                const QDateTime& timestamp,
                                const QStringList& entityIds,
                                const QString& tenantId);
    void onPortfoliosLoaded();
    void onBooksLoaded();
    void onCounterpartiesLoaded();

private:
    void setupUi();
    void setupToolbar();
    void setupTree();
    void setupTradePanel();
    void setupConnections();
    void setupEventSubscriptions();
    void rebuildTree();
    void updateBreadcrumb(const PortfolioTreeNode* node);

    // Fetch result types
    struct PortfolioFetchResult {
        bool success;
        std::vector<refdata::domain::portfolio> portfolios;
        QString error_message;
        QString error_details;
    };

    struct BookFetchResult {
        bool success;
        std::vector<refdata::domain::book> books;
        QString error_message;
        QString error_details;
    };

    struct CounterpartyFetchResult {
        bool success;
        std::unordered_map<std::string, CounterpartyInfo> cpty_map;
        QString error_message;
        QString error_details;
    };

    ClientManager* clientManager_;
    QString username_;

    // Layout
    QToolBar* toolbar_{nullptr};
    QAction* reloadAction_{nullptr};
    QSplitter* splitter_{nullptr};

    // Left: tree
    QTreeView* treeView_{nullptr};
    PortfolioBookTreeModel* treeModel_{nullptr};

    // Right: trade panel
    QLabel* breadcrumbLabel_{nullptr};
    QTableView* tradeTableView_{nullptr};
    PortfolioBookTradeModel* tradeModel_{nullptr};
    QSortFilterProxyModel* tradeProxyModel_{nullptr};
    PaginationWidget* paginationWidget_{nullptr};

    // Async watchers
    QFutureWatcher<PortfolioFetchResult>* portfolioWatcher_{nullptr};
    QFutureWatcher<BookFetchResult>* bookWatcher_{nullptr};
    QFutureWatcher<CounterpartyFetchResult>* counterpartyWatcher_{nullptr};

    // Data
    std::vector<refdata::domain::portfolio> portfolios_;
    std::vector<refdata::domain::book> books_;
    bool portfolios_loaded_{false};
    bool books_loaded_{false};
};

}

#endif
