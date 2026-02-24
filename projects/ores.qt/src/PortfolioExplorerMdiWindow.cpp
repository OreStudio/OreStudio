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
#include "ores.qt/PortfolioExplorerMdiWindow.hpp"

#include <QtConcurrent>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QToolButton>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata/messaging/portfolio_protocol.hpp"
#include "ores.refdata/messaging/book_protocol.hpp"
#include "ores.refdata/messaging/counterparty_protocol.hpp"
#include "ores.refdata/domain/counterparty.hpp"
#include "ores.trading/messaging/trade_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

PortfolioExplorerMdiWindow::PortfolioExplorerMdiWindow(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username) {

    setupUi();
    setupConnections();
    setupEventSubscriptions();

    reload();
}

void PortfolioExplorerMdiWindow::setupUi() {
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

void PortfolioExplorerMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    reloadAction_->setToolTip(tr("Refresh portfolio/book tree"));
    connect(reloadAction_, &QAction::triggered, this,
            &PortfolioExplorerMdiWindow::reload);

    initializeStaleIndicator(reloadAction_,
        IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        tr("+ Portfolio"));
    toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Book, IconUtils::DefaultIconColor),
        tr("+ Book"));

    toolbar_->addSeparator();

    auto* editAction = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction->setEnabled(false);

    auto* deleteAction = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction->setEnabled(false);

    auto* historyAction = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction->setEnabled(false);
}

void PortfolioExplorerMdiWindow::setupTree() {
    treeModel_ = new PortfolioExplorerTreeModel(this);

    treeView_ = new QTreeView(splitter_);
    treeView_->setModel(treeModel_);
    treeView_->setHeaderHidden(true);
    treeView_->setSelectionMode(QAbstractItemView::SingleSelection);
    treeView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    treeView_->setAnimated(true);
    treeView_->setMinimumWidth(200);

    splitter_->addWidget(treeView_);
}

void PortfolioExplorerMdiWindow::setupTradePanel() {
    auto* right_panel = new QWidget(splitter_);
    auto* right_layout = new QVBoxLayout(right_panel);
    right_layout->setContentsMargins(0, 0, 0, 0);
    right_layout->setSpacing(2);

    breadcrumbBar_ = new QWidget(right_panel);
    auto* bl = new QHBoxLayout(breadcrumbBar_);
    bl->setContentsMargins(0, 0, 0, 0);
    bl->setSpacing(2);
    right_layout->addWidget(breadcrumbBar_);

    tradeModel_ = new PortfolioExplorerTradeModel(clientManager_, this);
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
    splitter_->setStretchFactor(1, 3);
}

void PortfolioExplorerMdiWindow::setupConnections() {
    // Tree selection
    connect(treeView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &PortfolioExplorerMdiWindow::onTreeSelectionChanged);

    // Trade model signals
    connect(tradeModel_, &PortfolioExplorerTradeModel::dataLoaded,
            this, [this]() {
        paginationWidget_->update_state(
            static_cast<std::uint32_t>(tradeModel_->rowCount()),
            tradeModel_->total_available_count());
    });

    // Pagination signals
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

    // Async watchers
    portfolioWatcher_ = new QFutureWatcher<PortfolioFetchResult>(this);
    connect(portfolioWatcher_, &QFutureWatcher<PortfolioFetchResult>::finished,
            this, &PortfolioExplorerMdiWindow::onPortfoliosLoaded);

    bookWatcher_ = new QFutureWatcher<BookFetchResult>(this);
    connect(bookWatcher_, &QFutureWatcher<BookFetchResult>::finished,
            this, &PortfolioExplorerMdiWindow::onBooksLoaded);

    counterpartyWatcher_ = new QFutureWatcher<CounterpartyFetchResult>(this);
    connect(counterpartyWatcher_,
            &QFutureWatcher<CounterpartyFetchResult>::finished,
            this, &PortfolioExplorerMdiWindow::onCounterpartiesLoaded);
}

void PortfolioExplorerMdiWindow::setupEventSubscriptions() {
    if (!clientManager_)
        return;

    connect(clientManager_, &ClientManager::notificationReceived,
            this, &PortfolioExplorerMdiWindow::onNotificationReceived);

    auto subscribe_all = [this]() {
        clientManager_->subscribeToEvent(std::string{book_event});
        clientManager_->subscribeToEvent(std::string{portfolio_event});
        clientManager_->subscribeToEvent(std::string{trade_event});
    };

    connect(clientManager_, &ClientManager::loggedIn,
            this, [subscribe_all]() { subscribe_all(); });

    connect(clientManager_, &ClientManager::reconnected,
            this, [subscribe_all]() { subscribe_all(); });

    if (clientManager_->isConnected())
        subscribe_all();
}

void PortfolioExplorerMdiWindow::reload() {
    clearStaleIndicator();

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot reload: not connected.";
        return;
    }

    portfolios_loaded_ = false;
    books_loaded_ = false;

    // Fetch portfolios
    QPointer<PortfolioExplorerMdiWindow> self = this;
    portfolioWatcher_->setFuture(
        QtConcurrent::run([self]() -> PortfolioFetchResult {
            return exception_helper::wrap_async_fetch<PortfolioFetchResult>(
                [&]() -> PortfolioFetchResult {
                    if (!self || !self->clientManager_)
                        return {.success = false, .portfolios = {},
                                .error_message = "Model destroyed"};

                    refdata::messaging::get_portfolios_request req;
                    req.limit = 1000;
                    auto result = self->clientManager_->
                        process_authenticated_request(std::move(req));
                    if (!result)
                        return {.success = false, .portfolios = {},
                                .error_message = QString::fromStdString(
                                    "Failed to fetch portfolios: " +
                                    comms::net::to_string(result.error()))};

                    return {.success = true,
                            .portfolios = std::move(result->portfolios),
                            .error_message = {}};
                }, "portfolios");
        }));

    // Fetch books
    bookWatcher_->setFuture(
        QtConcurrent::run([self]() -> BookFetchResult {
            return exception_helper::wrap_async_fetch<BookFetchResult>(
                [&]() -> BookFetchResult {
                    if (!self || !self->clientManager_)
                        return {.success = false, .books = {},
                                .error_message = "Model destroyed"};

                    refdata::messaging::get_books_request req;
                    req.limit = 1000;
                    auto result = self->clientManager_->
                        process_authenticated_request(std::move(req));
                    if (!result)
                        return {.success = false, .books = {},
                                .error_message = QString::fromStdString(
                                    "Failed to fetch books: " +
                                    comms::net::to_string(result.error()))};

                    return {.success = true,
                            .books = std::move(result->books),
                            .error_message = {}};
                }, "books");
        }));

    // Fetch counterparties for display
    counterpartyWatcher_->setFuture(
        QtConcurrent::run([self]() -> CounterpartyFetchResult {
            return exception_helper::wrap_async_fetch<CounterpartyFetchResult>(
                [&]() -> CounterpartyFetchResult {
                    if (!self || !self->clientManager_)
                        return {.success = false, .cpty_map = {},
                                .error_message = "Model destroyed"};

                    refdata::messaging::get_counterparties_request req;
                    req.limit = 100'000;
                    auto result = self->clientManager_->
                        process_authenticated_request(std::move(req));
                    if (!result)
                        return {.success = false, .cpty_map = {},
                                .error_message = QString::fromStdString(
                                    "Failed to fetch counterparties: " +
                                    comms::net::to_string(result.error()))};

                    std::unordered_map<std::string, CounterpartyInfo> cpty_map;
                    for (const auto& c : result->counterparties) {
                        cpty_map[boost::uuids::to_string(c.id)] = {
                            .short_code = c.short_code,
                            .full_name = c.full_name};
                    }
                    return {.success = true,
                            .cpty_map = std::move(cpty_map),
                            .error_message = {}};
                }, "counterparties");
        }));
}

void PortfolioExplorerMdiWindow::onPortfoliosLoaded() {
    const auto result = portfolioWatcher_->result();
    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load portfolios: "
                                   << result.error_message.toStdString();
        return;
    }

    portfolios_ = std::move(result.portfolios);
    portfolios_loaded_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Loaded " << portfolios_.size()
                               << " portfolios.";

    if (portfolios_loaded_ && books_loaded_)
        rebuildTree();
}

void PortfolioExplorerMdiWindow::onBooksLoaded() {
    const auto result = bookWatcher_->result();
    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load books: "
                                   << result.error_message.toStdString();
        return;
    }

    books_ = std::move(result.books);
    books_loaded_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Loaded " << books_.size() << " books.";

    if (portfolios_loaded_ && books_loaded_)
        rebuildTree();
}

void PortfolioExplorerMdiWindow::onCounterpartiesLoaded() {
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

void PortfolioExplorerMdiWindow::collectBookUuids(
    const QModelIndex& parent, QList<boost::uuids::uuid>& uuids) {
    for (int r = 0; r < treeModel_->rowCount(parent); ++r) {
        auto idx = treeModel_->index(r, 0, parent);
        const auto* node = treeModel_->node_from_index(idx);
        if (!node)
            continue;
        if (node->kind == PortfolioTreeNode::Kind::Book)
            uuids.append(node->book.id);
        else
            collectBookUuids(idx, uuids);
    }
}

void PortfolioExplorerMdiWindow::rebuildTree() {
    BOOST_LOG_SEV(lg(), debug) << "Rebuilding portfolio/book tree.";

    // Disconnect and discard any pending count watchers from a prior reload
    for (auto* w : countWatchers_) {
        w->disconnect();
        w->deleteLater();
    }
    countWatchers_.clear();

    const QString party_name = clientManager_
        ? clientManager_->currentPartyName() : tr("Party");
    treeModel_->load(party_name, portfolios_, books_);
    treeView_->expandAll();
    updateBreadcrumb(nullptr);

    // Fetch trade counts for each book in the background
    QList<boost::uuids::uuid> book_ids;
    collectBookUuids({}, book_ids);

    QPointer<PortfolioExplorerMdiWindow> self = this;
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

void PortfolioExplorerMdiWindow::onTreeSelectionChanged(
    const QItemSelection& selected, const QItemSelection& /*deselected*/) {

    if (selected.isEmpty()) {
        tradeModel_->set_filter(std::nullopt, std::nullopt);
        updateBreadcrumb(nullptr);
        tradeModel_->refresh();
        return;
    }

    const auto index = selected.indexes().first();
    const auto filter = treeModel_->selected_filter(index);
    const auto* node = treeModel_->node_from_index(index);

    tradeModel_->set_filter(filter.book_id, filter.portfolio_id);
    updateBreadcrumb(node);
    tradeModel_->refresh();
}

void PortfolioExplorerMdiWindow::updateBreadcrumb(
    const PortfolioTreeNode* node) {
    // Clear all existing breadcrumb widgets
    QLayout* layout = breadcrumbBar_->layout();
    while (QLayoutItem* item = layout->takeAt(0)) {
        if (QWidget* w = item->widget())
            w->deleteLater();
        delete item;
    }

    auto* bl = static_cast<QHBoxLayout*>(layout);

    auto add_button = [&](const QString& label, bool bold,
                          auto on_click) {
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
        // No selection: just a bold "Trades" label
        add_button(tr("Trades"), true,
            [this]() { treeView_->clearSelection(); });
        bl->addStretch();
        return;
    }

    // Collect nodes from selected node up to root, then reverse
    QList<const PortfolioTreeNode*> path;
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
        if (n->kind == PortfolioTreeNode::Kind::Party)
            name = n->party_name;
        else if (n->kind == PortfolioTreeNode::Kind::Portfolio)
            name = QString::fromStdString(n->portfolio.name);
        else
            name = QString::fromStdString(n->book.name);

        const QModelIndex idx = indices[i];
        add_button(name, is_last,
            [this, idx]() { treeView_->setCurrentIndex(idx); });
    }

    bl->addStretch();
}

void PortfolioExplorerMdiWindow::onNotificationReceived(
    const QString& eventType,
    const QDateTime& /*timestamp*/,
    const QStringList& /*entityIds*/,
    const QString& /*tenantId*/) {

    if (eventType == QLatin1String(book_event) ||
        eventType == QLatin1String(portfolio_event) ||
        eventType == QLatin1String(trade_event)) {
        markAsStale();
    }
}

}
