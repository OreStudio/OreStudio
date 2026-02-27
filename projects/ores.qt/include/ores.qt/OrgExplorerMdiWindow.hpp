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
#ifndef ORES_QT_ORG_EXPLORER_MDI_WINDOW_HPP
#define ORES_QT_ORG_EXPLORER_MDI_WINDOW_HPP

#include <vector>
#include <QList>
#include <QLabel>
#include <QAction>
#include <QToolBar>
#include <QSplitter>
#include <QTreeView>
#include <QWidget>
#include <QTableView>
#include <QDateTime>
#include <QFutureWatcher>
#include <QItemSelection>
#include <QSortFilterProxyModel>
#include <QToolButton>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/PaginationWidget.hpp"
#include "ores.qt/OrgExplorerTreeModel.hpp"
#include "ores.qt/OrgExplorerTradeModel.hpp"
#include "ores.refdata/domain/business_unit.hpp"
#include "ores.refdata/domain/book.hpp"

namespace ores::qt {

class BusinessUnitController;
class BookController;
class TradeController;

/**
 * @brief MDI window showing the organisational hierarchy with a filtered trade table.
 *
 * Displays the Party → BusinessUnit → Book tree for the session party on the
 * left. Selecting any node scopes the trade table on the right to that subtree.
 * The stale indicator pulses when business unit, book, or trade changes arrive.
 */
class OrgExplorerMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.org_explorer_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    // Event names for subscription
    static constexpr const char* book_event = "ores.refdata.book_changed";
    static constexpr const char* business_unit_event =
        "ores.refdata.business_unit_changed";
    static constexpr const char* trade_event = "ores.trading.trade_changed";

public:
    explicit OrgExplorerMdiWindow(
        ClientManager* clientManager,
        BusinessUnitController* businessUnitController,
        BookController* bookController,
        TradeController* tradeController,
        const QString& username,
        QWidget* parent = nullptr);
    ~OrgExplorerMdiWindow() override = default;

    QSize sizeHint() const override { return QSize(1200, 700); }

public slots:
    /**
     * @brief Reload all data from server.
     *
     * Fires parallel async fetches for business units, books, and counterparties.
     */
    void reload() override;

signals:
    void statusChanged(const QString& msg);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh organisational tree");
    }

private slots:
    void onTreeSelectionChanged(const QItemSelection& selected,
                                const QItemSelection& deselected);
    void onNotificationReceived(const QString& eventType,
                                const QDateTime& timestamp,
                                const QStringList& entityIds,
                                const QString& tenantId);
    void onUnitsLoaded();
    void onBooksLoaded();
    void onCounterpartiesLoaded();
    void onEditSelected();
    void onHistorySelected();
    void onTradeDoubleClicked(const QModelIndex& index);
    void updateActionStates();

private:
    void setupUi();
    void setupToolbar();
    void setupTree();
    void setupTradePanel();
    void setupConnections();
    void setupEventSubscriptions();
    void rebuildTree();
    void updateBreadcrumb(const OrgTreeNode* node);
    void collectBookUuids(const QModelIndex& parent,
                          QList<boost::uuids::uuid>& uuids);

    // Fetch result types
    struct UnitFetchResult {
        bool success;
        std::vector<refdata::domain::business_unit> units;
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

    struct CountResult {
        boost::uuids::uuid book_id;
        std::uint32_t count{0};
        bool success{false};
        QString error_message;
        QString error_details;
    };

    ClientManager* clientManager_;
    QString username_;

    // Controllers (not owned — lifetime guaranteed by MainWindow)
    BusinessUnitController* businessUnitController_{nullptr};
    BookController* bookController_{nullptr};
    TradeController* tradeController_{nullptr};

    // Layout
    QToolBar* toolbar_{nullptr};
    QAction* reloadAction_{nullptr};
    QAction* editAction_{nullptr};
    QAction* historyAction_{nullptr};

    QSplitter* splitter_{nullptr};

    // Left: tree
    QTreeView* treeView_{nullptr};
    OrgExplorerTreeModel* treeModel_{nullptr};

    // Right: trade panel
    QWidget* breadcrumbBar_{nullptr};
    QTableView* tradeTableView_{nullptr};
    OrgExplorerTradeModel* tradeModel_{nullptr};
    QSortFilterProxyModel* tradeProxyModel_{nullptr};
    PaginationWidget* paginationWidget_{nullptr};

    // Async watchers
    QFutureWatcher<UnitFetchResult>* unitWatcher_{nullptr};
    QFutureWatcher<BookFetchResult>* bookWatcher_{nullptr};
    QFutureWatcher<CounterpartyFetchResult>* counterpartyWatcher_{nullptr};
    QList<QFutureWatcher<CountResult>*> countWatchers_;

    // Data
    std::vector<refdata::domain::business_unit> units_;
    std::vector<refdata::domain::book> books_;
    bool units_loaded_{false};
    bool books_loaded_{false};
};

}

#endif
