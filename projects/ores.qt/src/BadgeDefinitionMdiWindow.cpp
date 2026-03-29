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
#include "ores.qt/BadgeDefinitionMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.dq.api/messaging/badge_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

BadgeDefinitionMdiWindow::BadgeDefinitionMdiWindow(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      paginationWidget_(nullptr),
      reloadAction_(nullptr),
      addAction_(nullptr),
      editAction_(nullptr),
      deleteAction_(nullptr),
      historyAction_(nullptr) {

    setupUi();
    setupConnections();

    // Initial load
    reload();
}

void BadgeDefinitionMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void BadgeDefinitionMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this,
            &EntityListMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new badge definition"));
    connect(addAction_, &QAction::triggered, this,
            &BadgeDefinitionMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected badge definition"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &BadgeDefinitionMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected badge definition"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &BadgeDefinitionMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View badge definition history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &BadgeDefinitionMdiWindow::viewHistorySelected);
}

void BadgeDefinitionMdiWindow::setupTable() {
    model_ = new ClientBadgeDefinitionModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->verticalHeader()->setVisible(false);

    initializeTableSettings(tableView_, model_,
        "BadgeDefinitionListWindow",
        {ClientBadgeDefinitionModel::Description},
        {900, 400}, 1);
}

void BadgeDefinitionMdiWindow::setupConnections() {
    connect(model_, &ClientBadgeDefinitionModel::dataLoaded,
            this, &BadgeDefinitionMdiWindow::onDataLoaded);
    connect(model_, &ClientBadgeDefinitionModel::loadError,
            this, &BadgeDefinitionMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &BadgeDefinitionMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &BadgeDefinitionMdiWindow::onDoubleClicked);

    connect(paginationWidget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t size) {
        model_->set_page_size(size);
        model_->refresh();
    });

    connect(paginationWidget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
            model_->refresh();
        }
    });

    connect(paginationWidget_, &PaginationWidget::page_requested,
            this, [this](std::uint32_t offset, std::uint32_t limit) {
        model_->load_page(offset, limit);
    });
}

void BadgeDefinitionMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading badge definitions";
    emit statusChanged(tr("Loading badge definitions..."));
    model_->refresh();
}

void BadgeDefinitionMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 badge definitions").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(
        loaded < static_cast<int>(total) && total > 0 && total <= 1000);
}

void BadgeDefinitionMdiWindow::onLoadError(const QString& error_message,
                                          const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void BadgeDefinitionMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void BadgeDefinitionMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* definition = model_->getDefinition(sourceIndex.row())) {
        emit showDefinitionDetails(*definition);
    }
}

void BadgeDefinitionMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void BadgeDefinitionMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new badge definition requested";
    emit addNewRequested();
}

void BadgeDefinitionMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* definition = model_->getDefinition(sourceIndex.row())) {
        emit showDefinitionDetails(*definition);
    }
}

void BadgeDefinitionMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* definition = model_->getDefinition(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showDefinitionHistory for code: "
                                   << definition->code;
        emit showDefinitionHistory(*definition);
    }
}

void BadgeDefinitionMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete badge definition while disconnected.");
        return;
    }

    std::vector<std::string> codes;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* definition = model_->getDefinition(sourceIndex.row())) {
            codes.push_back(definition->code);
        }
    }

    if (codes.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid badge definitions to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << codes.size()
                               << " badge definitions";

    QString confirmMessage;
    if (codes.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete badge definition '%1'?")
            .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 badge definitions?")
            .arg(codes.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Badge Definition",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    struct BatchDeleteResult {
        bool success;
        std::string message;
        std::vector<std::string> codes;
    };

    QPointer<BadgeDefinitionMdiWindow> self = this;

    auto task = [self, codes]() -> BatchDeleteResult {
        if (!self) return {false, "Window closed", {}};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << codes.size() << " badge definitions";

        dq::messaging::delete_badge_definition_request request;
        request.codes = codes;
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, response_result.error(), {}};
        }

        return {response_result->success, response_result->message, codes};
    };

    auto* watcher = new QFutureWatcher<BatchDeleteResult>(self);
    connect(watcher, &QFutureWatcher<BatchDeleteResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        self->model_->refresh();

        if (result.success) {
            for (const auto& code : result.codes) {
                BOOST_LOG_SEV(lg(), debug) << "Badge Definition deleted: " << code;
                emit self->definitionDeleted(QString::fromStdString(code));
            }
            const int count = static_cast<int>(result.codes.size());
            QString msg = count == 1
                ? "Successfully deleted 1 badge definition"
                : QString("Successfully deleted %1 badge definitions").arg(count);
            emit self->statusChanged(msg);
        } else {
            BOOST_LOG_SEV(lg(), error) << "Batch delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorOccurred(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<BatchDeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
