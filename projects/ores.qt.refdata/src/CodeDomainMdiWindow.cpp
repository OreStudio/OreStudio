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
#include "ores.qt/CodeDomainMdiWindow.hpp"

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

CodeDomainMdiWindow::CodeDomainMdiWindow(
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

void CodeDomainMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void CodeDomainMdiWindow::setupToolbar() {
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
    addAction_->setToolTip(tr("Add new code domain"));
    connect(addAction_, &QAction::triggered, this,
            &CodeDomainMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected code domain"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &CodeDomainMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected code domain"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &CodeDomainMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View code domain history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &CodeDomainMdiWindow::viewHistorySelected);
}

void CodeDomainMdiWindow::setupTable() {
    model_ = new ClientCodeDomainModel(clientManager_, this);
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
        "CodeDomainListWindow",
        {ClientCodeDomainModel::Description},
        {900, 400}, 1);
}

void CodeDomainMdiWindow::setupConnections() {
    connect(model_, &ClientCodeDomainModel::dataLoaded,
            this, &CodeDomainMdiWindow::onDataLoaded);
    connect(model_, &ClientCodeDomainModel::loadError,
            this, &CodeDomainMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &CodeDomainMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &CodeDomainMdiWindow::onDoubleClicked);

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

void CodeDomainMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading code domains";
    emit statusChanged(tr("Loading code domains..."));
    model_->refresh();
}

void CodeDomainMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 code domains").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(
        loaded < static_cast<int>(total) && total > 0 && total <= 1000);
}

void CodeDomainMdiWindow::onLoadError(const QString& error_message,
                                          const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void CodeDomainMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void CodeDomainMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* domain = model_->getDomain(sourceIndex.row())) {
        emit showDomainDetails(*domain);
    }
}

void CodeDomainMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void CodeDomainMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new code domain requested";
    emit addNewRequested();
}

void CodeDomainMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* domain = model_->getDomain(sourceIndex.row())) {
        emit showDomainDetails(*domain);
    }
}

void CodeDomainMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* domain = model_->getDomain(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showDomainHistory for code: "
                                   << domain->code;
        emit showDomainHistory(*domain);
    }
}

void CodeDomainMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete code domain while disconnected.");
        return;
    }

    std::vector<std::string> codes;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* domain = model_->getDomain(sourceIndex.row())) {
            codes.push_back(domain->code);
        }
    }

    if (codes.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid code domains to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << codes.size()
                               << " code domains";

    QString confirmMessage;
    if (codes.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete code domain '%1'?")
            .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 code domains?")
            .arg(codes.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Code Domain",
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

    QPointer<CodeDomainMdiWindow> self = this;

    auto task = [self, codes]() -> BatchDeleteResult {
        if (!self) return {false, "Window closed", {}};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << codes.size() << " code domains";

        dq::messaging::delete_code_domain_request request;
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
                BOOST_LOG_SEV(lg(), debug) << "Code Domain deleted: " << code;
                emit self->domainDeleted(QString::fromStdString(code));
            }
            const int count = static_cast<int>(result.codes.size());
            QString msg = count == 1
                ? "Successfully deleted 1 code domain"
                : QString("Successfully deleted %1 code domains").arg(count);
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
