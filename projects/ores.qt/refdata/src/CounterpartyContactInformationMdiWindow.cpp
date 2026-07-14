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
#include "ores.qt/CounterpartyContactInformationMdiWindow.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/counterparty_contact_information_protocol.hpp"
#include <QFutureWatcher>
#include <QHeaderView>
#include <QMessageBox>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

CounterpartyContactInformationMdiWindow::CounterpartyContactInformationMdiWindow(
    ClientManager* clientManager, const QString& username, ImageCache* imageCache, QWidget* parent)
    : EntityListMdiWindow(parent)
    , clientManager_(clientManager)
    , username_(username)
    , imageCache_(imageCache)
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

void CounterpartyContactInformationMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void CounterpartyContactInformationMdiWindow::setupToolbar() {
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
    addAction_->setToolTip(tr("Add new counterparty contact information"));
    connect(
        addAction_, &QAction::triggered, this, &CounterpartyContactInformationMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor), tr("Edit"));
    editAction_->setToolTip(tr("Edit selected counterparty contact information"));
    editAction_->setEnabled(false);
    connect(editAction_,
            &QAction::triggered,
            this,
            &CounterpartyContactInformationMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected counterparty contact information"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_,
            &QAction::triggered,
            this,
            &CounterpartyContactInformationMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor), tr("History"));
    historyAction_->setToolTip(tr("View counterparty contact information history"));
    historyAction_->setEnabled(false);
    connect(historyAction_,
            &QAction::triggered,
            this,
            &CounterpartyContactInformationMdiWindow::viewHistorySelected);
}

void CounterpartyContactInformationMdiWindow::setupTable() {
    model_ = new ClientCounterpartyContactInformationModel(clientManager_, this);
    model_->setImageCache(imageCache_);
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
    tableView_->setIconSize(single_flag_icon_size());

    using cs = column_style;
    auto* delegate = new EntityItemDelegate(
        {
            cs::text_left,
            cs::text_left,
            cs::text_left,
            cs::icon_text_left,
            cs::text_left,
            cs::text_left,
            cs::mono_center,
            cs::text_left,
            cs::text_left,
        },
        tableView_);
    tableView_->setItemDelegate(delegate);

    initializeTableSettings(
        tableView_, model_, "CounterpartyContactInformationListWindow", {}, {900, 400}, 1);
}

void CounterpartyContactInformationMdiWindow::setupConnections() {
    connect(model_,
            &ClientCounterpartyContactInformationModel::dataLoaded,
            this,
            &CounterpartyContactInformationMdiWindow::onDataLoaded);
    connect(model_,
            &ClientCounterpartyContactInformationModel::loadError,
            this,
            &CounterpartyContactInformationMdiWindow::onLoadError);

    connect(tableView_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this,
            &CounterpartyContactInformationMdiWindow::onSelectionChanged);
    connect(tableView_,
            &QTableView::doubleClicked,
            this,
            &CounterpartyContactInformationMdiWindow::onDoubleClicked);

    connect(
        paginationWidget_, &PaginationWidget::page_size_changed, this, [this](std::uint32_t size) {
            model_->set_page_size(size);
            model_->refresh();
        });

    connect(paginationWidget_, &PaginationWidget::load_all_requested, this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
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

void CounterpartyContactInformationMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading counterparty contact informations";
    clearStaleIndicator();
    emit statusChanged(tr("Loading counterparty contact informations..."));
    model_->refresh();
}

void CounterpartyContactInformationMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(
        tr("Loaded %1 of %2 counterparty contact informations").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(loaded < static_cast<int>(total) && total > 0 &&
                                            total <= 1000);
}

void CounterpartyContactInformationMdiWindow::onLoadError(const QString& error_message,
                                                          const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void CounterpartyContactInformationMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void CounterpartyContactInformationMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* counterpartyContactInformation = model_->getInformation(sourceIndex.row())) {
        emit showInformationDetails(*counterpartyContactInformation);
    }
}

void CounterpartyContactInformationMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void CounterpartyContactInformationMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new counterparty contact information requested";
    emit addNewRequested();
}

void CounterpartyContactInformationMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* counterpartyContactInformation = model_->getInformation(sourceIndex.row())) {
        emit showInformationDetails(*counterpartyContactInformation);
    }
}

void CounterpartyContactInformationMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* counterpartyContactInformation = model_->getInformation(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showInformationHistory for code: "
                                   << counterpartyContactInformation->contact_type;
        emit showInformationHistory(*counterpartyContactInformation);
    }
}

void CounterpartyContactInformationMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete counterparty contact information while disconnected.");
        return;
    }

    std::vector<std::string> ids;
    std::vector<std::string> codes; // For display purposes
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* counterpartyContactInformation = model_->getInformation(sourceIndex.row())) {
            ids.push_back(boost::uuids::to_string(counterpartyContactInformation->id));
            codes.push_back(counterpartyContactInformation->contact_type);
        }
    }

    if (ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid counterparty contact informations to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << ids.size()
                               << " counterparty contact informations";

    QString confirmMessage;
    if (ids.size() == 1) {
        confirmMessage =
            QString("Are you sure you want to delete counterparty contact information '%1'?")
                .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage =
            QString("Are you sure you want to delete %1 counterparty contact informations?")
                .arg(ids.size());
    }

    auto reply = MessageBoxHelper::question(this,
                                            "Delete Counterparty Contact Information",
                                            confirmMessage,
                                            QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<CounterpartyContactInformationMdiWindow> self = this;
    using DeleteResult = std::vector<std::tuple<std::string, std::string, bool, std::string>>;

    auto task = [self, ids, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self)
            return {};

        BOOST_LOG_SEV(lg(), debug)
            << "Making delete request for " << ids.size() << " counterparty contact informations";

        refdata::messaging::delete_counterparty_contact_information_request request;
        request.ids = ids;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Failed to communicate with server"});
            }
            return results;
        }

        for (std::size_t i = 0; i < ids.size(); ++i) {
            results.push_back(
                {ids[i], codes[i], response_result->success, response_result->message});
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

        for (const auto& [id, code, success, message] : results) {
            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Counterparty Contact Information deleted: " << code;
                success_count++;
                emit self->counterpartyContactInformationDeleted(QString::fromStdString(code));
            } else {
                BOOST_LOG_SEV(lg(), error)
                    << "Counterparty Contact Information deletion failed: " << code << " - "
                    << message;
                failure_count++;
                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(message);
                }
            }
        }

        self->model_->refresh();

        if (failure_count == 0) {
            QString msg = success_count == 1 ?
                              "Successfully deleted 1 counterparty contact information" :
                              QString("Successfully deleted %1 counterparty contact informations")
                                  .arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                              .arg(failure_count)
                              .arg(failure_count == 1 ? "counterparty contact information" :
                                                        "counterparty contact informations")
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
