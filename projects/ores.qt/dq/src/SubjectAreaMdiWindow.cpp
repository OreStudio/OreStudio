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
#include "ores.qt/SubjectAreaMdiWindow.hpp"
#include ""
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include <QFutureWatcher>
#include <QHeaderView>
#include <QMessageBox>
#include <QVBoxLayout>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

SubjectAreaMdiWindow::SubjectAreaMdiWindow(ClientManager* clientManager,
                                           const QString& username,
                                           QWidget* parent)
    : EntityListMdiWindow(parent)
    , clientManager_(clientManager)
    , username_(username)
    , toolbar_(nullptr)
    , tableView_(nullptr)
    , model_(nullptr)
    , proxyModel_(nullptr)
    , paginationWidget_(nullptr)
    , reloadAction_(nullptr)
    , addAction_(nullptr)
    , editAction_(nullptr)
    , deleteAction_(nullptr)
    , historyAction_(nullptr) {

    setupUi();
    setupConnections();
    reload();
}

void SubjectAreaMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void SubjectAreaMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this, &EntityListMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), tr("Add"));
    addAction_->setToolTip(tr("Add new subject area"));
    connect(addAction_, &QAction::triggered, this, &SubjectAreaMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor), tr("Edit"));
    editAction_->setToolTip(tr("Edit selected subject area"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this, &SubjectAreaMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected subject area"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this, &SubjectAreaMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor), tr("History"));
    historyAction_->setToolTip(tr("View subject area history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this, &SubjectAreaMdiWindow::viewHistorySelected);
}

void SubjectAreaMdiWindow::setupTable() {
    model_ = new ClientSubjectAreaModel(clientManager_, this);
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


    initializeTableSettings(tableView_, model_, "", {}, {900, 400}, );
}

void SubjectAreaMdiWindow::setupConnections() {
    connect(model_, &ClientSubjectAreaModel::dataLoaded, this, &SubjectAreaMdiWindow::onDataLoaded);
    connect(model_, &ClientSubjectAreaModel::loadError, this, &SubjectAreaMdiWindow::onLoadError);

    connect(tableView_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this,
            &SubjectAreaMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked, this, &SubjectAreaMdiWindow::onDoubleClicked);

    connect(
        paginationWidget_, &PaginationWidget::page_size_changed, this, [this](std::uint32_t size) {
            model_->set_page_size(size);
            model_->refresh();
        });

    connect(paginationWidget_, &PaginationWidget::load_all_requested, this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
            paginationWidget_->reset_page();
            model_->refresh();
        }
    });

    connect(
        paginationWidget_,
        &PaginationWidget::page_requested,
        this,
        [this](std::uint32_t offset, std::uint32_t limit) { model_->load_page(offset, limit); });

    connectModel(model_);
}

void SubjectAreaMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading subject areas";
    clearStaleIndicator();
    emit statusChanged(tr("Loading subject areas..."));
    model_->load_page(paginationWidget_->current_offset(), paginationWidget_->page_size());
}

void SubjectAreaMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 subject areas").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(loaded < static_cast<int>(total) && total > 0 &&
                                            total <= 1000);
}

void SubjectAreaMdiWindow::onLoadError(const QString& error_message, const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void SubjectAreaMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void SubjectAreaMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* = model_->getArea(sourceIndex.row())) {
        emit showAreaDetails(*);
    }
}

void SubjectAreaMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void SubjectAreaMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new subject area requested";
    emit addNewRequested();
}

void SubjectAreaMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* = model_->getArea(sourceIndex.row())) {
        emit showAreaDetails(*);
    }
}

void SubjectAreaMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* = model_->getArea(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showAreaHistory for code: " <<->;
        emit showAreaHistory(*);
    }
}

void SubjectAreaMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete subject area while disconnected.");
        return;
    }

    std::vector<std::string> codes;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* = model_->getArea(sourceIndex.row())) {
            codes.push_back(->);
        }
    }

    if (codes.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid subject areas to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << codes.size() << " subject areas";

    QString confirmMessage;
    if (codes.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete subject area '%1'?")
                             .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage =
            QString("Are you sure you want to delete %1 subject areas?").arg(codes.size());
    }

    auto reply = MessageBoxHelper::question(
        this, "Delete Subject Area", confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<SubjectAreaMdiWindow> self = this;
    using DeleteResult = std::vector<std::pair<std::string, std::pair<bool, std::string>>>;

    auto task = [self, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self)
            return {};

        BOOST_LOG_SEV(lg(), debug)
            << "Making delete request for " << codes.size() << " subject areas";

        request;
        request.codes = codes;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (const auto& code : codes) {
                results.push_back({code, {false, "Failed to communicate with server"}});
            }
            return results;
        }

        for (const auto& code : codes) {
            results.push_back({code, {response_result->success, response_result->message}});
        }

        return results;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& [code, result] : results) {
            if (result.first) {
                BOOST_LOG_SEV(lg(), debug) << "Subject Area deleted: " << code;
                success_count++;
                emit self->Deleted(QString::fromStdString(code));
            } else {
                BOOST_LOG_SEV(lg(), error)
                    << "Subject Area deletion failed: " << code << " - " << result.second;
                failure_count++;
                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(result.second);
                }
            }
        }

        self->model_->load_page(self->paginationWidget_->current_offset(),
                                self->paginationWidget_->page_size());

        if (failure_count == 0) {
            QString msg = success_count == 1 ?
                              "Successfully deleted 1 subject area" :
                              QString("Successfully deleted %1 subject areas").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                              .arg(failure_count)
                              .arg(failure_count == 1 ? "subject area" : "subject areas")
                              .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg =
                QString("Deleted %1, failed to delete %2").arg(success_count).arg(failure_count);
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}


}
