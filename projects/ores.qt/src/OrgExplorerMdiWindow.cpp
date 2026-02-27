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
#include "ores.qt/OrgExplorerMdiWindow.hpp"

#include <QtConcurrent>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QVBoxLayout>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.qt/BusinessUnitController.hpp"
#include "ores.qt/BookController.hpp"
#include "ores.qt/TradeController.hpp"
#include "ores.refdata/messaging/business_unit_protocol.hpp"
#include "ores.refdata/messaging/book_protocol.hpp"
#include "ores.refdata/messaging/counterparty_protocol.hpp"
#include "ores.refdata/domain/counterparty.hpp"
#include "ores.trading/messaging/trade_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

OrgExplorerMdiWindow::OrgExplorerMdiWindow(
    ClientManager* clientManager,
    BusinessUnitController* businessUnitController,
    BookController* bookController,
    TradeController* tradeController,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      businessUnitController_(businessUnitController),
      bookController_(bookController),
      tradeController_(tradeController) {

    setupUi();
    setupConnections();
    setupEventSubscriptions();

    reload();
}

void OrgExplorerMdiWindow::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(4, 4, 4, 4);
    layout->setSpacing(2);

    setupToolbar();
    layout->addWidget(toolbar_);

    splitter_ = new QSplitter(Qt::Horizontal, this);

    setupTree();
    setupTradePanel();

    layout->addWidget(splitter_, 1);
}

void OrgExplorerMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    reloadAction_->setToolTip(tr("Refresh organisational tree"));
    connect(reloadAction_, &QAction::triggered, this,
            &OrgExplorerMdiWindow::reload);

    initializeStaleIndicator(reloadAction_,
        IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected item"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered,
            this, &OrgExplorerMdiWindow::onEditSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View history of selected item"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered,
            this, &OrgExplorerMdiWindow::onHistorySelected);
}

void OrgExplorerMdiWindow::setupTree() {
    treeModel_ = new OrgExplorerTreeModel(this);

    treeView_ = new QTreeView(splitter_);
    treeView_->setModel(treeModel_);
    treeView_->setHeaderHidden(true);
    treeView_->setSelectionMode(QAbstractItemView::SingleSelection);
    treeView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    treeView_->setAnimated(true);
    treeView_->setMinimumWidth(200);

    splitter_->addWidget(treeView_);
}

void OrgExplorerMdiWindow::setupTradePanel() {
    auto* right_panel = new QWidget(splitter_);
    auto* right_layout = new QVBoxLayout(right_panel);
    right_layout->setContentsMargins(0, 0, 0, 0);
    right_layout->setSpacing(2);

    breadcrumbBar_ = new QWidget(right_panel);
    auto* bl = new QHBoxLayout(breadcrumbBar_);
    bl->setContentsMargins(0, 0, 0, 0);
    bl->setSpacing(2);
    right_layout->addWidget(breadcrumbBar_);

    tradeModel_ = new OrgExplorerTradeModel(clientManager_, this);
    tradeProxyModel_ = new QSortFilterProxyModel(this);
    tradeProxyModel_->setSourceModel(tradeModel_);
    tradeProxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tradeTableView_ = new QTableView(right_panel);
    tradeTableView_->setModel(tradeProxyModel_);
    tradeTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tradeTableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tradeTableView_->setSortingEnabled(true);
    tradeTableView_->setAlternatingRowColors(true);
    tradeTableView_->verticalHeader()->setVisible(false);
    tradeTableView_->horizontalHeader()->setStretchLastSection(true);
    right_layout->addWidget(tradeTableView_, 1);

    paginationWidget_ = new PaginationWidget(right_panel);
    right_layout->addWidget(paginationWidget_);

    splitter_->addWidget(right_panel);
    splitter_->setStretchFactor(0, 1);
    splitter_->setStretchFactor(1, 2);

    initializeTableSettings(tradeTableView_, tradeModel_,
        "OrgExplorer", {}, {1100, 600}, 1, splitter_);
}

void OrgExplorerMdiWindow::setupConnections() {
    connect(treeView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &OrgExplorerMdiWindow::onTreeSelectionChanged);

    connect(tradeTableView_, &QTableView::doubleClicked,
            this, &OrgExplorerMdiWindow::onTradeDoubleClicked);

    connect(tradeModel_, &OrgExplorerTradeModel::dataLoaded,
            this, [this]() {
        paginationWidget_->update_state(
            static_cast<std::uint32_t>(tradeModel_->rowCount()),
            tradeModel_->total_available_count());
    });

    connect(paginationWidget_, &PaginationWidget::page_requested,
            this, [this](std::uint32_t offset, std::uint32_t limit) {
        tradeModel_->load_page(offset, limit);
    });

    connect(paginationWidget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t /*size*/) {
        tradeModel_->refresh();
    });

    connect(paginationWidget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        const auto total = tradeModel_->total_available_count();
        if (total > 0 && total <= 1000)
            tradeModel_->load_page(0, total);
    });

    unitWatcher_ = new QFutureWatcher<UnitFetchResult>(this);
    connect(unitWatcher_, &QFutureWatcher<UnitFetchResult>::finished,
            this, &OrgExplorerMdiWindow::onUnitsLoaded);

    bookWatcher_ = new QFutureWatcher<BookFetchResult>(this);
    connect(bookWatcher_, &QFutureWatcher<BookFetchResult>::finished,
            this, &OrgExplorerMdiWindow::onBooksLoaded);

    counterpartyWatcher_ = new QFutureWatcher<CounterpartyFetchResult>(this);
    connect(counterpartyWatcher_,
            &QFutureWatcher<CounterpartyFetchResult>::finished,
            this, &OrgExplorerMdiWindow::onCounterpartiesLoaded);
}

void OrgExplorerMdiWindow::setupEventSubscriptions() {
    if (!clientManager_)
        return;

    connect(clientManager_, &ClientManager::notificationReceived,
            this, &OrgExplorerMdiWindow::onNotificationReceived);

    auto subscribe_all = [this]() {
        clientManager_->subscribeToEvent(std::string{book_event});
        clientManager_->subscribeToEvent(std::string{business_unit_event});
        clientManager_->subscribeToEvent(std::string{trade_event});
    };

    connect(clientManager_, &ClientManager::loggedIn,
            this, [subscribe_all]() { subscribe_all(); });

    connect(clientManager_, &ClientManager::reconnected,
            this, [subscribe_all]() { subscribe_all(); });

    if (clientManager_->isConnected())
        subscribe_all();
}

void OrgExplorerMdiWindow::reload() {
    clearStaleIndicator();

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot reload: not connected.";
        return;
    }

    units_loaded_ = false;
    books_loaded_ = false;

    QPointer<OrgExplorerMdiWindow> self = this;

    // Fetch business units
    unitWatcher_->setFuture(
        QtConcurrent::run([self]() -> UnitFetchResult {
            return exception_helper::wrap_async_fetch<UnitFetchResult>(
                [&]() -> UnitFetchResult {
                    if (!self || !self->clientManager_)
                        return {.success = false, .units = {},
                                .error_message = "Model destroyed",
                                .error_details = {}};

                    refdata::messaging::get_business_units_request req;
                    req.limit = 10'000;
                    auto result = self->clientManager_->
                        process_authenticated_request(std::move(req));
                    if (!result)
                        return {.success = false, .units = {},
                                .error_message = QString::fromStdString(
                                    "Failed to fetch business units: " +
                                    comms::net::to_string(result.error())),
                                .error_details = {}};

                    return {.success = true,
                            .units = std::move(result->business_units),
                            .error_message = {},
                            .error_details = {}};
                }, "business units");
        }));

    // Fetch books
    bookWatcher_->setFuture(
        QtConcurrent::run([self]() -> BookFetchResult {
            return exception_helper::wrap_async_fetch<BookFetchResult>(
                [&]() -> BookFetchResult {
                    if (!self || !self->clientManager_)
                        return {.success = false, .books = {},
                                .error_message = "Model destroyed",
                                .error_details = {}};

                    refdata::messaging::get_books_request req;
                    req.limit = 10'000;
                    auto result = self->clientManager_->
                        process_authenticated_request(std::move(req));
                    if (!result)
                        return {.success = false, .books = {},
                                .error_message = QString::fromStdString(
                                    "Failed to fetch books: " +
                                    comms::net::to_string(result.error())),
                                .error_details = {}};

                    return {.success = true,
                            .books = std::move(result->books),
                            .error_message = {},
                            .error_details = {}};
                }, "books");
        }));

    // Fetch counterparties for display
    counterpartyWatcher_->setFuture(
        QtConcurrent::run([self]() -> CounterpartyFetchResult {
            return exception_helper::wrap_async_fetch<CounterpartyFetchResult>(
                [&]() -> CounterpartyFetchResult {
                    if (!self || !self->clientManager_)
                        return {.success = false, .cpty_map = {},
                                .error_message = "Model destroyed",
                                .error_details = {}};

                    refdata::messaging::get_counterparties_request req;
                    req.limit = 100'000;
                    auto result = self->clientManager_->
                        process_authenticated_request(std::move(req));
                    if (!result)
                        return {.success = false, .cpty_map = {},
                                .error_message = QString::fromStdString(
                                    "Failed to fetch counterparties: " +
                                    comms::net::to_string(result.error())),
                                .error_details = {}};

                    std::unordered_map<std::string, CounterpartyInfo> cpty_map;
                    for (const auto& c : result->counterparties) {
                        cpty_map[boost::uuids::to_string(c.id)] = {
                            .short_code = c.short_code,
                            .full_name = c.full_name};
                    }
                    return {.success = true,
                            .cpty_map = std::move(cpty_map),
                            .error_message = {},
                            .error_details = {}};
                }, "counterparties");
        }));
}

void OrgExplorerMdiWindow::onUnitsLoaded() {
    const auto result = unitWatcher_->result();
    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load business units: "
                                   << result.error_message.toStdString();
        return;
    }

    units_ = std::move(result.units);
    units_loaded_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Loaded " << units_.size()
                               << " business units.";

    if (units_loaded_ && books_loaded_)
        rebuildTree();
}

void OrgExplorerMdiWindow::onBooksLoaded() {
    const auto result = bookWatcher_->result();
    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load books: "
                                   << result.error_message.toStdString();
        return;
    }

    books_ = std::move(result.books);
    books_loaded_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Loaded " << books_.size() << " books.";

    if (units_loaded_ && books_loaded_)
        rebuildTree();
}

void OrgExplorerMdiWindow::onCounterpartiesLoaded() {
    const auto result = counterpartyWatcher_->result();
    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load counterparties: "
                                   << result.error_message.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loaded " << result.cpty_map.size()
                               << " counterparties.";
    tradeModel_->set_counterparty_map(
        // NOLINTNEXTLINE(performance-move-const-arg)
        std::unordered_map<std::string, CounterpartyInfo>(result.cpty_map));
}

void OrgExplorerMdiWindow::collectBookUuids(
    const QModelIndex& parent, QList<boost::uuids::uuid>& uuids) {
    for (int r = 0; r < treeModel_->rowCount(parent); ++r) {
        auto idx = treeModel_->index(r, 0, parent);
        const auto* node = treeModel_->node_from_index(idx);
        if (!node)
            continue;
        if (node->kind == OrgTreeNode::Kind::Book)
            uuids.append(node->book.id);
        else
            collectBookUuids(idx, uuids);
    }
}

void OrgExplorerMdiWindow::rebuildTree() {
    BOOST_LOG_SEV(lg(), debug) << "Rebuilding organisational tree.";

    // Disconnect and discard any pending count watchers from a prior reload
    for (auto* w : countWatchers_) {
        w->disconnect();
        w->deleteLater();
    }
    countWatchers_.clear();

    const QString party_name = clientManager_
        ? clientManager_->currentPartyName() : tr("Party");
    treeModel_->load(party_name, units_, books_);
    treeView_->expandAll();
    updateBreadcrumb(nullptr);
    updateActionStates();

    // Fetch trade counts for each book in the background
    QList<boost::uuids::uuid> book_ids;
    collectBookUuids({}, book_ids);

    QPointer<OrgExplorerMdiWindow> self = this;
    for (const auto& book_id : book_ids) {
        auto* watcher = new QFutureWatcher<CountResult>(this);
        countWatchers_.append(watcher);

        connect(watcher, &QFutureWatcher<CountResult>::finished, this,
            [this, watcher]() {
                countWatchers_.removeOne(watcher);
                const auto result = watcher->result();
                if (result.success)
                    treeModel_->set_trade_count(result.book_id, result.count);
                watcher->deleteLater();
            });

        const auto bid = book_id;
        watcher->setFuture(QtConcurrent::run([self, bid]() -> CountResult {
            return exception_helper::wrap_async_fetch<CountResult>(
                [&]() -> CountResult {
                    if (!self || !self->clientManager_)
                        return {.book_id = bid, .count = 0,
                                .success = false,
                                .error_message = "Destroyed",
                                .error_details = {}};

                    trading::messaging::get_trades_request req;
                    req.book_id = bid;
                    req.limit = 0;
                    req.offset = 0;

                    auto result = self->clientManager_->
                        process_authenticated_request(std::move(req));
                    if (!result)
                        return {.book_id = bid, .count = 0,
                                .success = false,
                                .error_message = "Failed to get count",
                                .error_details = {}};

                    return {.book_id = bid,
                            .count = result->total_available_count,
                            .success = true,
                            .error_message = {},
                            .error_details = {}};
                }, "book trade count");
        }));
    }
}

void OrgExplorerMdiWindow::onTreeSelectionChanged(
    const QItemSelection& selected, const QItemSelection& /*deselected*/) {

    if (selected.isEmpty()) {
        tradeModel_->set_filter(std::nullopt, std::nullopt);
        updateBreadcrumb(nullptr);
        tradeModel_->refresh();
        updateActionStates();
        return;
    }

    const auto index = selected.indexes().first();
    const auto filter = treeModel_->selected_filter(index);
    const auto* node = treeModel_->node_from_index(index);

    tradeModel_->set_filter(filter.book_id, filter.business_unit_id);
    updateBreadcrumb(node);
    tradeModel_->refresh();
    updateActionStates();
}

void OrgExplorerMdiWindow::updateBreadcrumb(const OrgTreeNode* node) {
    QLayout* layout = breadcrumbBar_->layout();
    while (QLayoutItem* item = layout->takeAt(0)) {
        if (QWidget* w = item->widget())
            w->deleteLater();
        delete item;
    }

    auto* bl = static_cast<QHBoxLayout*>(layout);

    auto add_button = [&](const QString& label, bool bold, auto on_click) {
        auto* btn = new QToolButton(breadcrumbBar_);
        btn->setText(label);
        btn->setAutoRaise(true);
        btn->setCursor(Qt::PointingHandCursor);
        if (bold)
            btn->setStyleSheet("font-weight: bold; border: none; background: none;");
        else
            btn->setStyleSheet("border: none; background: none;");
        connect(btn, &QToolButton::clicked, on_click);
        bl->addWidget(btn);
    };

    auto add_separator = [&]() {
        auto* sep = new QLabel(QStringLiteral("›"), breadcrumbBar_);
        bl->addWidget(sep);
    };

    if (!node) {
        add_button(tr("Trades"), true,
            [this]() { treeView_->clearSelection(); });
        bl->addStretch();
        return;
    }

    // Collect nodes from selected node up to root, then reverse
    QList<const OrgTreeNode*> path;
    const auto* current = node;
    while (current) {
        path.prepend(current);
        current = current->parent;
    }

    // Build QModelIndex for each path node top-down
    QList<QModelIndex> indices;
    QModelIndex parent_idx;
    for (const auto* n : path) {
        QModelIndex idx = treeModel_->index(n->row_in_parent, 0, parent_idx);
        indices.append(idx);
        parent_idx = idx;
    }

    // "Trades" root — always clears selection
    add_button(tr("Trades"), false,
        [this]() { treeView_->clearSelection(); });

    for (int i = 0; i < path.size(); ++i) {
        add_separator();

        const auto* n = path[i];
        const bool is_last = (i == path.size() - 1);

        QString name;
        if (n->kind == OrgTreeNode::Kind::Party)
            name = n->party_name;
        else if (n->kind == OrgTreeNode::Kind::BusinessUnit)
            name = QString::fromStdString(n->unit.unit_name);
        else
            name = QString::fromStdString(n->book.name);

        const QModelIndex idx = indices[i];
        add_button(name, is_last,
            [this, idx]() { treeView_->setCurrentIndex(idx); });
    }

    bl->addStretch();
}

void OrgExplorerMdiWindow::onNotificationReceived(
    const QString& eventType,
    const QDateTime& /*timestamp*/,
    const QStringList& /*entityIds*/,
    const QString& /*tenantId*/) {

    if (eventType == QLatin1String(book_event) ||
        eventType == QLatin1String(business_unit_event) ||
        eventType == QLatin1String(trade_event)) {
        markAsStale();
    }
}

void OrgExplorerMdiWindow::updateActionStates() {
    const auto* node = treeModel_->node_from_index(treeView_->currentIndex());
    const bool has_editable = node &&
        (node->kind == OrgTreeNode::Kind::BusinessUnit ||
         node->kind == OrgTreeNode::Kind::Book);

    editAction_->setEnabled(has_editable);
    historyAction_->setEnabled(has_editable);
}

void OrgExplorerMdiWindow::onEditSelected() {
    const auto* node = treeModel_->node_from_index(treeView_->currentIndex());
    if (!node) return;
    if (node->kind == OrgTreeNode::Kind::BusinessUnit && businessUnitController_)
        businessUnitController_->openEdit(node->unit);
    else if (node->kind == OrgTreeNode::Kind::Book && bookController_)
        bookController_->openEdit(node->book);
}

void OrgExplorerMdiWindow::onHistorySelected() {
    const auto* node = treeModel_->node_from_index(treeView_->currentIndex());
    if (!node) return;
    if (node->kind == OrgTreeNode::Kind::BusinessUnit && businessUnitController_)
        businessUnitController_->openHistory(node->unit);
    else if (node->kind == OrgTreeNode::Kind::Book && bookController_)
        bookController_->openHistory(node->book);
}

void OrgExplorerMdiWindow::onTradeDoubleClicked(const QModelIndex& index) {
    if (!index.isValid() || !tradeController_)
        return;

    const auto sourceIndex = tradeProxyModel_->mapToSource(index);
    const auto* trade = tradeModel_->get_trade(sourceIndex.row());
    if (!trade)
        return;

    tradeController_->openEdit(*trade);
}

}
