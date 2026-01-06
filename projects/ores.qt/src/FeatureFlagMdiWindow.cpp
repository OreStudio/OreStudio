/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/FeatureFlagMdiWindow.hpp"

#include <vector>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QtWidgets/QHeaderView>
#include <QMessageBox>
#include <QSortFilterProxyModel>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.variability/messaging/feature_flags_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::message_type;
using namespace ores::telemetry::log;

FeatureFlagMdiWindow::
FeatureFlagMdiWindow(ClientManager* clientManager,
                     const QString& username,
                     QWidget* parent)
    : QWidget(parent),
      verticalLayout_(new QVBoxLayout(this)),
      featureFlagTableView_(new QTableView(this)),
      toolBar_(new QToolBar(this)),
      reloadAction_(new QAction("Reload", this)),
      pulseTimer_(new QTimer(this)),
      addAction_(new QAction("Add", this)),
      editAction_(new QAction("Edit", this)),
      deleteAction_(new QAction("Delete", this)),
      featureFlagModel_(std::make_unique<ClientFeatureFlagModel>(clientManager)),
      proxyModel_(new QSortFilterProxyModel(this)),
      clientManager_(clientManager),
      username_(username) {

    BOOST_LOG_SEV(lg(), debug) << "Creating feature flag MDI window";

    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    const QColor iconColor(220, 220, 220);

    // Setup reload action with normal and stale icons
    setupReloadAction();
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    addAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_add_20_regular.svg", iconColor));
    addAction_->setToolTip("Add new feature flag");
    connect(addAction_, &QAction::triggered, this, &FeatureFlagMdiWindow::addNew);
    toolBar_->addAction(addAction_);

    editAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_edit_20_regular.svg", iconColor));
    editAction_->setToolTip("Edit selected feature flag");
    connect(editAction_, &QAction::triggered, this,
        &FeatureFlagMdiWindow::editSelected);
    toolBar_->addAction(editAction_);

    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_delete_20_regular.svg", iconColor));
    deleteAction_->setToolTip("Delete selected feature flag(s)");
    connect(deleteAction_, &QAction::triggered, this,
        &FeatureFlagMdiWindow::deleteSelected);
    toolBar_->addAction(deleteAction_);

    verticalLayout_->addWidget(toolBar_);
    verticalLayout_->addWidget(featureFlagTableView_);

    featureFlagTableView_->setObjectName("featureFlagTableView");
    featureFlagTableView_->setAlternatingRowColors(true);
    featureFlagTableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    featureFlagTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    featureFlagTableView_->setWordWrap(false);

    proxyModel_->setSourceModel(featureFlagModel_.get());
    featureFlagTableView_->setModel(proxyModel_);
    featureFlagTableView_->setSortingEnabled(true);
    featureFlagTableView_->sortByColumn(0, Qt::AscendingOrder);

    QHeaderView* horizontalHeader(featureFlagTableView_->horizontalHeader());
    featureFlagTableView_->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
    horizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    // Connect signals
    connect(featureFlagModel_.get(), &ClientFeatureFlagModel::dataLoaded,
            this, &FeatureFlagMdiWindow::onDataLoaded);
    connect(featureFlagModel_.get(), &ClientFeatureFlagModel::loadError,
            this, &FeatureFlagMdiWindow::onLoadError);
    connect(featureFlagTableView_, &QTableView::doubleClicked,
            this, &FeatureFlagMdiWindow::onRowDoubleClicked);
    connect(featureFlagTableView_->selectionModel(),
        &QItemSelectionModel::selectionChanged,
            this, &FeatureFlagMdiWindow::onSelectionChanged);

    // Connect connection state signals
    if (clientManager_) {
        connect(clientManager_, &ClientManager::connected, this,
            &FeatureFlagMdiWindow::onConnectionStateChanged);
        connect(clientManager_, &ClientManager::disconnected, this,
            &FeatureFlagMdiWindow::onConnectionStateChanged);
    }

    updateActionStates();

    emit statusChanged("Loading feature flags...");

    // Initial load
    if (clientManager_->isConnected()) {
        featureFlagModel_->refresh();
    } else {
        emit statusChanged("Disconnected - Offline");
        toolBar_->setEnabled(false);
    }
}

FeatureFlagMdiWindow::~FeatureFlagMdiWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying feature flag MDI window";

    // Cancel any pending operations. The QPointer in the lambdas ensures
    // they safely handle this window being destroyed without blocking.
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
    }
}

void FeatureFlagMdiWindow::onConnectionStateChanged() {
    const bool connected = clientManager_ && clientManager_->isConnected();
    toolBar_->setEnabled(connected);

    if (connected) {
        emit statusChanged("Connected");
    } else {
        emit statusChanged("Disconnected - Offline");
    }
}

void FeatureFlagMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reload requested";
    if (!clientManager_->isConnected()) {
        emit statusChanged("Cannot reload - Disconnected");
        return;
    }
    emit statusChanged("Reloading feature flags...");
    clearStaleIndicator();
    featureFlagModel_->refresh();
}

void FeatureFlagMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new feature flag requested";
    emit addNewRequested();
}

void FeatureFlagMdiWindow::onDataLoaded() {
    const auto loaded = featureFlagModel_->rowCount();

    const QString message = QString("Loaded %1 feature flags").arg(loaded);
    emit statusChanged(message);
    BOOST_LOG_SEV(lg(), debug) << "Feature flag data loaded successfully: "
                               << loaded << " flags";

    if (featureFlagModel_->rowCount() > 0 &&
        featureFlagTableView_->selectionModel()->selectedRows().isEmpty()) {
        featureFlagTableView_->selectRow(0);
        BOOST_LOG_SEV(lg(), debug) << "Auto-selected first row";
    }
}

void FeatureFlagMdiWindow::onLoadError(const QString& error_message) {
    emit errorOccurred(error_message);
    BOOST_LOG_SEV(lg(), error) << "Error loading feature flags: "
                               << error_message.toStdString();

    MessageBoxHelper::critical(this, "Load Error", error_message);
}

void FeatureFlagMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    if (!index.isValid()) {
        return;
    }

    // Map proxy index to source index
    const QModelIndex sourceIndex = proxyModel_->mapToSource(index);
    const auto* flag = featureFlagModel_->getFeatureFlag(sourceIndex.row());
    if (!flag) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get feature flag for row: "
                                  << sourceIndex.row();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Emitting showFeatureFlagDetails for: "
                               << flag->name;
    emit showFeatureFlagDetails(*flag);
}

void FeatureFlagMdiWindow::onSelectionChanged() {
    const int selection_count = featureFlagTableView_->selectionModel()->selectedRows().count();
    updateActionStates();
    emit selectionChanged(selection_count);
}

void FeatureFlagMdiWindow::editSelected() {
    const auto selected = featureFlagTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    onRowDoubleClicked(selected.first());
}

void FeatureFlagMdiWindow::deleteSelected() {
    const auto selected = featureFlagTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete feature flag while disconnected.");
        return;
    }

    std::vector<std::string> names;
    for (const auto& index : selected) {
        const QModelIndex sourceIndex = proxyModel_->mapToSource(index);
        const auto* flag = featureFlagModel_->getFeatureFlag(sourceIndex.row());
        if (flag)
            names.push_back(flag->name);
    }

    if (names.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid feature flags to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << names.size()
                               << " feature flags";

    QString confirmMessage;
    if (names.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete feature flag '%1'?")
            .arg(QString::fromStdString(names[0]));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 feature flags?")
            .arg(names.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Feature Flag",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user.";
        return;
    }

    QPointer<FeatureFlagMdiWindow> self = this;
    using DeleteResult = std::vector<std::pair<std::string, std::pair<bool, std::string>>>;

    auto task = [self, names]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        for (const auto& name : names) {
            BOOST_LOG_SEV(lg(), debug) << "Deleting feature flag: " << name;

            variability::messaging::delete_feature_flag_request request;
            request.name = name;
            auto payload = request.serialize();

            comms::messaging::frame request_frame(
                message_type::delete_feature_flag_request,
                0, std::move(payload)
            );

            auto response_result = self->clientManager_->sendRequest(
                std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send delete request";
                results.push_back({name,
                    {false, "Failed to communicate with server"}});
                continue;
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                results.push_back({name,
                    {false, "Failed to decompress server response"}});
                continue;
            }

            auto response = variability::messaging::delete_feature_flag_response::
                deserialize(*payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                results.push_back({name, {false, "Invalid server response"}});
                continue;
            }

            results.push_back({name, {response->success, response->error_message}});
        }

        return results;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& [name, result] : results) {
            auto [success, message] = result;

            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Feature flag deleted successfully: "
                                           << name;
                success_count++;
                emit self->featureFlagDeleted(QString::fromStdString(name));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Feature flag deletion failed: "
                                           << name << " - " << message;
                failure_count++;
                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(message);
                }
            }
        }

        self->featureFlagModel_->refresh();
        if (failure_count == 0) {
            QString msg = success_count == 1
                ? "Successfully deleted 1 feature flag"
                : QString("Successfully deleted %1 feature flags").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "feature flag" : "feature flags")
                .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg = QString("Deleted %1 %2, failed to delete %3 %4")
                .arg(success_count)
                .arg(success_count == 1 ? "feature flag" : "feature flags")
                .arg(failure_count)
                .arg(failure_count == 1 ? "feature flag" : "feature flags");
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

QSize FeatureFlagMdiWindow::sizeHint() const {
    const int minimumWidth = 700;
    const int minimumHeight = 400;

    QSize baseSize = QWidget::sizeHint();

    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void FeatureFlagMdiWindow::updateActionStates() {
    const int selection_count = featureFlagTableView_
        ->selectionModel()->selectedRows().count();
    const bool hasSingleSelection = selection_count == 1;
    const bool hasSelection = selection_count > 0;

    editAction_->setEnabled(hasSingleSelection);
    deleteAction_->setEnabled(hasSelection);
}

void FeatureFlagMdiWindow::setupReloadAction() {
    const QColor normalColor(220, 220, 220);
    const QColor staleColor(255, 165, 0);

    normalReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", normalColor);
    staleReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", staleColor);

    reloadAction_->setIcon(normalReloadIcon_);
    reloadAction_->setToolTip("Reload feature flags from server");
    connect(reloadAction_, &QAction::triggered, this, &FeatureFlagMdiWindow::reload);

    connect(pulseTimer_, &QTimer::timeout, this, [this]() {
        pulseState_ = !pulseState_;
        reloadAction_->setIcon(pulseState_ ? staleReloadIcon_ : normalReloadIcon_);

        pulseCount_++;
        if (pulseCount_ >= 6) {
            pulseTimer_->stop();
            reloadAction_->setIcon(staleReloadIcon_);
        }
    });
}

void FeatureFlagMdiWindow::startPulseAnimation() {
    pulseCount_ = 0;
    pulseState_ = false;
    pulseTimer_->start(500);
}

void FeatureFlagMdiWindow::stopPulseAnimation() {
    pulseTimer_->stop();
    reloadAction_->setIcon(normalReloadIcon_);
}

void FeatureFlagMdiWindow::markAsStale() {
    if (!isStale_) {
        isStale_ = true;
        reloadAction_->setToolTip("Data changed on server - click to reload");
        startPulseAnimation();
        BOOST_LOG_SEV(lg(), info) << "Feature flag data marked as stale";
    }
}

void FeatureFlagMdiWindow::clearStaleIndicator() {
    if (isStale_) {
        isStale_ = false;
        stopPulseAnimation();
        reloadAction_->setToolTip("Reload feature flags from server");
        BOOST_LOG_SEV(lg(), debug) << "Stale indicator cleared";
    }
}

}
