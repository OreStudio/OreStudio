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
#include "ores.qt/PortfolioBookTreeMdiWindow.hpp"

#include <QtConcurrent>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata/messaging/portfolio_protocol.hpp"
#include "ores.refdata/messaging/book_protocol.hpp"
#include "ores.refdata/messaging/counterparty_protocol.hpp"
#include "ores.refdata/domain/counterparty.hpp"

namespace ores::qt {

using namespace ores::logging;

PortfolioBookTreeMdiWindow::PortfolioBookTreeMdiWindow(
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

void PortfolioBookTreeMdiWindow::setupUi() {
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

void PortfolioBookTreeMdiWindow::setupToolbar() {
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
            &PortfolioBookTreeMdiWindow::reload);

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

void PortfolioBookTreeMdiWindow::setupTree() {
    treeModel_ = new PortfolioBookTreeModel(this);

    treeView_ = new QTreeView(splitter_);
    treeView_->setModel(treeModel_);
    treeView_->setHeaderHidden(true);
    treeView_->setSelectionMode(QAbstractItemView::SingleSelection);
    treeView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    treeView_->setAnimated(true);
    treeView_->setMinimumWidth(200);

    splitter_->addWidget(treeView_);
}

void PortfolioBookTreeMdiWindow::setupTradePanel() {
    auto* right_panel = new QWidget(splitter_);
    auto* right_layout = new QVBoxLayout(right_panel);
    right_layout->setContentsMargins(0, 0, 0, 0);
    right_layout->setSpacing(2);

    breadcrumbLabel_ = new QLabel(tr("Trades"), right_panel);
    breadcrumbLabel_->setStyleSheet("font-weight: bold; padding: 2px;");
    right_layout->addWidget(breadcrumbLabel_);

    tradeModel_ = new PortfolioBookTradeModel(clientManager_, this);
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

void PortfolioBookTreeMdiWindow::setupConnections() {
    // Tree selection
    connect(treeView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &PortfolioBookTreeMdiWindow::onTreeSelectionChanged);

    // Trade model signals
    connect(tradeModel_, &PortfolioBookTradeModel::dataLoaded,
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
            this, &PortfolioBookTreeMdiWindow::onPortfoliosLoaded);

    bookWatcher_ = new QFutureWatcher<BookFetchResult>(this);
    connect(bookWatcher_, &QFutureWatcher<BookFetchResult>::finished,
            this, &PortfolioBookTreeMdiWindow::onBooksLoaded);

    counterpartyWatcher_ = new QFutureWatcher<CounterpartyFetchResult>(this);
    connect(counterpartyWatcher_,
            &QFutureWatcher<CounterpartyFetchResult>::finished,
            this, &PortfolioBookTreeMdiWindow::onCounterpartiesLoaded);
}

void PortfolioBookTreeMdiWindow::setupEventSubscriptions() {
    if (!clientManager_)
        return;

    connect(clientManager_, &ClientManager::notificationReceived,
            this, &PortfolioBookTreeMdiWindow::onNotificationReceived);

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

void PortfolioBookTreeMdiWindow::reload() {
    clearStaleIndicator();

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot reload: not connected.";
        return;
    }

    portfolios_loaded_ = false;
    books_loaded_ = false;

    // Fetch portfolios
    QPointer<PortfolioBookTreeMdiWindow> self = this;
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
                    req.limit = 5000;
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

void PortfolioBookTreeMdiWindow::onPortfoliosLoaded() {
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

void PortfolioBookTreeMdiWindow::onBooksLoaded() {
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

void PortfolioBookTreeMdiWindow::onCounterpartiesLoaded() {
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

void PortfolioBookTreeMdiWindow::rebuildTree() {
    BOOST_LOG_SEV(lg(), debug) << "Rebuilding portfolio/book tree.";
    treeModel_->load(portfolios_, books_);
    treeView_->expandAll();
    breadcrumbLabel_->setText(tr("Trades"));
}

void PortfolioBookTreeMdiWindow::onTreeSelectionChanged(
    const QItemSelection& selected, const QItemSelection& /*deselected*/) {

    if (selected.isEmpty()) {
        tradeModel_->set_filter(std::nullopt, std::nullopt);
        breadcrumbLabel_->setText(tr("Trades"));
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

void PortfolioBookTreeMdiWindow::updateBreadcrumb(
    const PortfolioTreeNode* node) {
    if (!node) {
        breadcrumbLabel_->setText(tr("Trades"));
        return;
    }

    // Build path from root to this node
    QStringList path;
    const auto* current = node;
    while (current) {
        QString name;
        if (current->kind == PortfolioTreeNode::Kind::Portfolio)
            name = QString::fromStdString(current->portfolio.name);
        else
            name = QString::fromStdString(current->book.name);
        path.prepend(name);
        current = current->parent;
    }
    breadcrumbLabel_->setText(tr("Trades") + " › " + path.join(" › "));
}

void PortfolioBookTreeMdiWindow::onNotificationReceived(
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
