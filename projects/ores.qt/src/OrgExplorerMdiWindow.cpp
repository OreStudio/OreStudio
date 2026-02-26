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
#include <QVBoxLayout>
#include <QHeaderView>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/BusinessUnitController.hpp"
#include "ores.qt/BookController.hpp"
#include "ores.refdata/messaging/business_unit_protocol.hpp"
#include "ores.refdata/messaging/book_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

OrgExplorerMdiWindow::OrgExplorerMdiWindow(
    ClientManager* clientManager,
    BusinessUnitController* businessUnitController,
    BookController* bookController,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      businessUnitController_(businessUnitController),
      bookController_(bookController) {

    setupUi();
    setupConnections();
    setupEventSubscriptions();

    reload();
}

void OrgExplorerMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(4, 4, 4, 4);
    layout->setSpacing(2);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTree();
    layout->addWidget(treeView_, 1);
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

    treeView_ = new QTreeView(this);
    treeView_->setModel(treeModel_);
    treeView_->setHeaderHidden(true);
    treeView_->setSelectionMode(QAbstractItemView::SingleSelection);
    treeView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    treeView_->setAnimated(true);
}

void OrgExplorerMdiWindow::setupConnections() {
    connect(treeView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &OrgExplorerMdiWindow::onTreeSelectionChanged);

    unitWatcher_ = new QFutureWatcher<UnitFetchResult>(this);
    connect(unitWatcher_, &QFutureWatcher<UnitFetchResult>::finished,
            this, &OrgExplorerMdiWindow::onUnitsLoaded);

    bookWatcher_ = new QFutureWatcher<BookFetchResult>(this);
    connect(bookWatcher_, &QFutureWatcher<BookFetchResult>::finished,
            this, &OrgExplorerMdiWindow::onBooksLoaded);
}

void OrgExplorerMdiWindow::setupEventSubscriptions() {
    if (!clientManager_)
        return;

    connect(clientManager_, &ClientManager::notificationReceived,
            this, &OrgExplorerMdiWindow::onNotificationReceived);

    auto subscribe_all = [this]() {
        clientManager_->subscribeToEvent(std::string{book_event});
        clientManager_->subscribeToEvent(std::string{business_unit_event});
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

void OrgExplorerMdiWindow::rebuildTree() {
    BOOST_LOG_SEV(lg(), debug) << "Rebuilding organisational tree.";

    const QString party_name = clientManager_
        ? clientManager_->currentPartyName() : tr("Party");
    treeModel_->load(party_name, units_, books_);
    treeView_->expandAll();
    updateActionStates();
}

void OrgExplorerMdiWindow::onTreeSelectionChanged(
    const QItemSelection& /*selected*/, const QItemSelection& /*deselected*/) {
    updateActionStates();
}

void OrgExplorerMdiWindow::onNotificationReceived(
    const QString& eventType,
    const QDateTime& /*timestamp*/,
    const QStringList& /*entityIds*/,
    const QString& /*tenantId*/) {

    if (eventType == QLatin1String(book_event) ||
        eventType == QLatin1String(business_unit_event)) {
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

}
