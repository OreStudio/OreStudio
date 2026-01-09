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
#include "ores.qt/FeatureFlagHistoryDialog.hpp"

#include <QIcon>
#include <QLabel>
#include <QDateTime>
#include <QScrollBar>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.variability/messaging/feature_flags_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;

const QIcon& FeatureFlagHistoryDialog::getHistoryIcon() const {
    static const QIcon historyIcon(":/icons/ic_fluent_history_20_regular.svg");
    return historyIcon;
}

FeatureFlagHistoryDialog::FeatureFlagHistoryDialog(QString name,
    ClientManager* clientManager, QWidget* parent)
    : QWidget(parent), ui_(new Ui::FeatureFlagHistoryDialog),
      clientManager_(clientManager),
      flagName_(std::move(name)),
      toolBar_(nullptr), reloadAction_(nullptr),
      openAction_(nullptr), revertAction_(nullptr) {

    BOOST_LOG_SEV(lg(), info) << "Creating feature flag history widget for: "
                              << flagName_.toStdString();

    ui_->setupUi(this);

    setupToolbar();

    connect(ui_->versionListWidget, &QTableWidget::currentCellChanged,
            this, [this](int currentRow, int, int, int) {
        onVersionSelected(currentRow);
    });

    // Double-click opens the version in read-only mode
    connect(ui_->versionListWidget, &QTableWidget::cellDoubleClicked,
            this, [this](int, int) {
        onOpenClicked();
    });

    ui_->versionListWidget->setAlternatingRowColors(true);
    ui_->versionListWidget->setSelectionMode(QAbstractItemView::SingleSelection);
    ui_->versionListWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui_->versionListWidget->resizeRowsToContents();

    QHeaderView* versionVerticalHeader = ui_->versionListWidget->verticalHeader();
    QHeaderView* versionHorizontalHeader = ui_->versionListWidget->horizontalHeader();
    versionVerticalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);
    versionHorizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    ui_->changesTableWidget->horizontalHeader()->setStretchLastSection(true);
    ui_->changesTableWidget->setColumnWidth(0, 200);
    ui_->changesTableWidget->setColumnWidth(1, 200);

    updateButtonStates();
}

FeatureFlagHistoryDialog::~FeatureFlagHistoryDialog() {
    BOOST_LOG_SEV(lg(), info) << "Destroying feature flag history widget";

    // Disconnect and cancel any active QFutureWatcher objects
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void FeatureFlagHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading feature flag history for: "
                              << flagName_.toStdString();

    variability::messaging::get_feature_flag_history_request request{flagName_.toStdString()};
    auto payload = request.serialize();

    frame request_frame(message_type::get_feature_flag_history_request,
        0, std::move(payload)
    );

    using HistoryResult = std::expected<frame, std::string>;
    QPointer<FeatureFlagHistoryDialog> self = this;
    QFuture<HistoryResult> future =
        QtConcurrent::run([self, frame = std::move(request_frame)]() mutable -> HistoryResult {
        if (!self->clientManager_ || !self->clientManager_->isConnected()) {
             return std::unexpected("Disconnected from server");
        }
        auto response_result = self->clientManager_->sendRequest(std::move(frame));
        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Could not obtain feature flag history: "
                                       << "Failed to communicate with server.";
            return std::unexpected("Failed to communicate with server");
        }
        return *response_result;
    });

    // Use watcher to handle results
    auto* watcher = new QFutureWatcher<HistoryResult>(self);
    connect(watcher, &QFutureWatcher<HistoryResult>::finished, self,
        [self, watcher]() {

        if (!self) return;
        auto result = watcher->result();
        watcher->deleteLater();

        if (!result) {
            self->onHistoryLoadError(QString::fromStdString(result.error()));
            return;
        }

        // Check if server sent an error_response instead
        if (result->header().type != message_type::get_feature_flag_history_response) {
            self->onHistoryLoadError(
                QString("Server does not support feature flag history: received message type %1")
                .arg(static_cast<int>(result->header().type)));
            return;
        }

        // Decompress payload
        auto payload_result = result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress history response";
            self->onHistoryLoadError("Failed to decompress server response");
            return;
        }

        auto response = variability::messaging::get_feature_flag_history_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Could not deserialise server response.";
            self->onHistoryLoadError("Invalid server response");
            return;
        }

        if (!response->success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            self->onHistoryLoadError(QString::fromStdString(response->message));
            return;
        }

        self->history_ = std::move(response->history);
        self->onHistoryLoaded();
    });

    watcher->setFuture(future);
}

void FeatureFlagHistoryDialog::onHistoryLoaded() {
    BOOST_LOG_SEV(lg(), info) << "History loaded successfully: "
                              << history_.size() << " versions";

    const QIcon& cachedIcon = getHistoryIcon();
    ui_->versionListWidget->setRowCount(0);
    ui_->versionListWidget->setRowCount(history_.size());

    for (int i = 0; i < static_cast<int>(history_.size()); ++i) {
        const auto& flag = history_[i];

        BOOST_LOG_SEV(lg(), trace) << "Displaying version [" << i << "]: "
                                   << "version=" << flag.version
                                   << ", recorded_by=" << flag.recorded_by;

        auto* versionItem =
            new QTableWidgetItem(QString::number(flag.version));
        auto* enabledItem =
            new QTableWidgetItem(flag.enabled ? "Yes" : "No");
        auto* recordedAtItem =
            new QTableWidgetItem(relative_time_helper::format(flag.recorded_at));
        auto* recordedByItem =
            new QTableWidgetItem(QString::fromStdString(flag.recorded_by));

        versionItem->setIcon(cachedIcon);

        ui_->versionListWidget->setItem(i, 0, versionItem);
        ui_->versionListWidget->setItem(i, 1, enabledItem);
        ui_->versionListWidget->setItem(i, 2, recordedAtItem);
        ui_->versionListWidget->setItem(i, 3, recordedByItem);
    }

    if (!history_.empty())
        ui_->versionListWidget->selectRow(0);

    if (!history_.empty()) {
        const auto& latest = history_[0];
        ui_->titleLabel->setText(QString("Feature Flag History: %1")
            .arg(QString::fromStdString(latest.name)));
    }

    updateButtonStates();

    emit statusChanged(QString("Loaded %1 versions")
        .arg(history_.size()));
}

void FeatureFlagHistoryDialog::onHistoryLoadError(const QString& error_msg) {
    BOOST_LOG_SEV(lg(), error) << "Error loading history: "
                               << error_msg.toStdString();

    emit errorOccurred(QString("Failed to load feature flag history: %1")
        .arg(error_msg));
    MessageBoxHelper::critical(this, "History Load Error",
        QString("Failed to load feature flag history:\n%1")
        .arg(error_msg));
}

void FeatureFlagHistoryDialog::onVersionSelected(int index) {
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    BOOST_LOG_SEV(lg(), trace) << "Version selected: " << index;

    displayChangesTab(index);
    displayFullDetailsTab(index);
}

void FeatureFlagHistoryDialog::displayChangesTab(int version_index) {
    ui_->changesTableWidget->setRowCount(0);

    if (version_index >= static_cast<int>(history_.size()))
        return;

    const auto& current = history_[version_index];

    // If this is the first (oldest) version, there's nothing to diff against so
    // leave the changes table empty
    if (version_index == static_cast<int>(history_.size()) - 1) {
        BOOST_LOG_SEV(lg(), trace) << "No previous version to diff against for oldest version";
        return;
    }

    // Calculate diff with previous version
    const auto& previous = history_[version_index + 1];

    BOOST_LOG_SEV(lg(), trace) << "Calculating diff between version "
                               << current.version << " and " << previous.version;

    auto diffs = calculateDiff(current, previous);

    BOOST_LOG_SEV(lg(), trace) << "Found " << diffs.size() << " differences";

    ui_->changesTableWidget->setRowCount(diffs.size());

    for (int i = 0; i < diffs.size(); ++i) {
        const auto& [field, values] = diffs[i];
        const auto& [old_val, new_val] = values;

        auto* fieldItem = new QTableWidgetItem(field);
        auto* oldItem = new QTableWidgetItem(old_val);
        auto* newItem = new QTableWidgetItem(new_val);

        ui_->changesTableWidget->setItem(i, 0, fieldItem);
        ui_->changesTableWidget->setItem(i, 1, oldItem);
        ui_->changesTableWidget->setItem(i, 2, newItem);
    }
}

void FeatureFlagHistoryDialog::displayFullDetailsTab(int version_index) {
    if (version_index >= static_cast<int>(history_.size()))
        return;

    const auto& flag = history_[version_index];

    ui_->nameValue->setText(QString::fromStdString(flag.name));
    ui_->enabledValue->setText(flag.enabled ? "Yes" : "No");
    ui_->descriptionValue->setText(QString::fromStdString(flag.description));
    ui_->versionNumberValue->setText(QString::number(flag.version));
    ui_->recordedByValue->setText(QString::fromStdString(flag.recorded_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(flag.recorded_at));
}

FeatureFlagHistoryDialog::DiffResult FeatureFlagHistoryDialog::
calculateDiff(const variability::domain::feature_flags& current,
    const variability::domain::feature_flags& previous) {

    DiffResult diffs;

    // Compare enabled flag
    if (current.enabled != previous.enabled) {
        diffs.append({"Enabled", {
            previous.enabled ? "Yes" : "No",
            current.enabled ? "Yes" : "No"
        }});
    }

    // Compare description
    if (current.description != previous.description) {
        diffs.append({"Description", {
            QString::fromStdString(previous.description),
            QString::fromStdString(current.description)
        }});
    }

    return diffs;
}

void FeatureFlagHistoryDialog::setupToolbar() {
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    const QColor iconColor(220, 220, 220);

    // Create Reload action
    reloadAction_ = new QAction("Reload", this);
    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", iconColor));
    reloadAction_->setToolTip("Reload history from server");
    connect(reloadAction_, &QAction::triggered, this,
        &FeatureFlagHistoryDialog::onReloadClicked);
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    // Create Open action
    openAction_ = new QAction("Open", this);
    openAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_edit_20_regular.svg", iconColor));
    openAction_->setToolTip("Open this version in read-only mode");
    connect(openAction_, &QAction::triggered, this,
        &FeatureFlagHistoryDialog::onOpenClicked);
    toolBar_->addAction(openAction_);

    // Create Revert action
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));
    revertAction_->setToolTip("Revert feature flag to this version");
    connect(revertAction_, &QAction::triggered, this,
        &FeatureFlagHistoryDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);

    // Add toolbar to layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);
}

void FeatureFlagHistoryDialog::updateButtonStates() {
    const int index = selectedVersionIndex();
    const bool hasSelection = index >= 0 &&
        index < static_cast<int>(history_.size());

    if (openAction_)
        openAction_->setEnabled(hasSelection);

    if (revertAction_)
        revertAction_->setEnabled(hasSelection);
}

int FeatureFlagHistoryDialog::selectedVersionIndex() const {
    return ui_->versionListWidget->currentRow();
}

void FeatureFlagHistoryDialog::onOpenClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    const auto& flag = history_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening feature flag version "
                              << flag.version << " in read-only mode";

    emit openVersionRequested(flag, flag.version);
}

void FeatureFlagHistoryDialog::onRevertClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    // Get the selected version and the previous (older) version
    const auto& current = history_[index];

    // If this is the oldest version, there's no previous version to revert to
    if (index == static_cast<int>(history_.size()) - 1) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot revert oldest version - no previous version exists";
        MessageBoxHelper::information(this, "Cannot Revert",
            "This is the oldest version. There is no previous version to revert to.");
        return;
    }

    // The "previous" version is the one we want to revert TO (the "old" side in the diff)
    const auto& previous = history_[index + 1];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert from version "
                              << current.version << " to version "
                              << previous.version;

    // Confirm with user
    auto reply = MessageBoxHelper::question(this, "Revert Feature Flag",
        QString("Are you sure you want to revert '%1' from version %2 back to version %3?\n\n"
                "This will create a new version with the data from version %3.")
            .arg(flagName_)
            .arg(current.version)
            .arg(previous.version),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Revert cancelled by user";
        return;
    }

    // Use the PREVIOUS version's data (the "old" side of the diff) for the revert.
    // Server handles versioning - we just send the data we want to restore.
    variability::domain::feature_flags flagToRevert = previous;
    emit revertVersionRequested(flagToRevert);
}

void FeatureFlagHistoryDialog::onReloadClicked() {
    BOOST_LOG_SEV(lg(), info) << "Reload requested for feature flag history: "
                              << flagName_.toStdString();
    emit statusChanged(QString("Reloading history for %1...").arg(flagName_));
    loadHistory();
}

QSize FeatureFlagHistoryDialog::sizeHint() const {
    QSize baseSize = QWidget::sizeHint();

    const int minimumWidth = 800;
    const int minimumHeight = 500;

    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void FeatureFlagHistoryDialog::markAsStale() {
    BOOST_LOG_SEV(lg(), info) << "Feature flag history marked as stale for: "
                              << flagName_.toStdString() << ", reloading...";

    emit statusChanged(QString("Feature flag %1 was modified - reloading history...")
        .arg(flagName_));

    // Reload history data
    loadHistory();
}

}
