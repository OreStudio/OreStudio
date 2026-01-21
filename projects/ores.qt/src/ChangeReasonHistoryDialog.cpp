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
#include "ores.qt/ChangeReasonHistoryDialog.hpp"

#include <QIcon>
#include <QDateTime>
#include <QScrollBar>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;

const QIcon& ChangeReasonHistoryDialog::getHistoryIcon() const {
    static const QIcon historyIcon = IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor);
    return historyIcon;
}

ChangeReasonHistoryDialog::ChangeReasonHistoryDialog(QString code,
    ClientManager* clientManager, QWidget* parent)
    : QWidget(parent), ui_(new Ui::ChangeReasonHistoryDialog),
      clientManager_(clientManager), code_(std::move(code)),
      toolBar_(nullptr), reloadAction_(nullptr),
      openAction_(nullptr), revertAction_(nullptr) {

    BOOST_LOG_SEV(lg(), info) << "Creating change reason history widget for: "
                              << code_.toStdString();

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

ChangeReasonHistoryDialog::~ChangeReasonHistoryDialog() {
    BOOST_LOG_SEV(lg(), info) << "Destroying change reason history widget";

    // Disconnect and cancel any active QFutureWatcher objects
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void ChangeReasonHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading change reason history for: "
                              << code_.toStdString();

    dq::messaging::get_change_reason_history_request request{code_.toStdString()};
    auto payload = request.serialize();

    frame request_frame(message_type::get_change_reason_history_request,
        0, std::move(payload)
    );

    using HistoryResult = std::expected<frame, std::string>;
    QPointer<ChangeReasonHistoryDialog> self = this;
    QFuture<HistoryResult> future =
        QtConcurrent::run([self, req_frame = std::move(request_frame)]() mutable -> HistoryResult {
        if (!self->clientManager_ || !self->clientManager_->isConnected()) {
             return std::unexpected("Disconnected from server");
        }
        auto response_result = self->clientManager_->sendRequest(std::move(req_frame));
        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Could not obtain change reason history: "
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
        if (result->header().type != message_type::get_change_reason_history_response) {
            self->onHistoryLoadError(
                QString("Server does not support change reason history: received message type %1")
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

        auto response = dq::messaging::get_change_reason_history_response::
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

        self->versions_ = std::move(response->versions);
        self->onHistoryLoaded();
    });

    watcher->setFuture(future);
}

void ChangeReasonHistoryDialog::onHistoryLoaded() {
    BOOST_LOG_SEV(lg(), info) << "History loaded successfully: "
                              << versions_.size() << " versions";

    const QIcon& cachedIcon = getHistoryIcon();
    ui_->versionListWidget->setRowCount(0);
    ui_->versionListWidget->setRowCount(versions_.size());

    for (int i = 0; i < static_cast<int>(versions_.size()); ++i) {
        const auto& version = versions_[i];

        auto* versionItem =
            new QTableWidgetItem(QString::number(version.version));
        auto* recordedAtItem =
            new QTableWidgetItem(relative_time_helper::format(version.recorded_at));
        auto* recordedByItem =
            new QTableWidgetItem(QString::fromStdString(version.recorded_by));
        auto* commentaryItem =
            new QTableWidgetItem(QString::fromStdString(version.change_commentary));

        versionItem->setIcon(cachedIcon);

        ui_->versionListWidget->setItem(i, 0, versionItem);
        ui_->versionListWidget->setItem(i, 1, recordedAtItem);
        ui_->versionListWidget->setItem(i, 2, recordedByItem);
        ui_->versionListWidget->setItem(i, 3, commentaryItem);
    }

    if (!versions_.empty())
        ui_->versionListWidget->selectRow(0);

    if (!versions_.empty()) {
        const auto& latest = versions_[0];
        ui_->titleLabel->setText(QString("Change Reason History: %1")
            .arg(QString::fromStdString(latest.code)));
    }

    updateButtonStates();

    emit statusChanged(QString("Loaded %1 versions")
        .arg(versions_.size()));
}

void ChangeReasonHistoryDialog::onHistoryLoadError(const QString& error_msg) {
    BOOST_LOG_SEV(lg(), error) << "Error loading history: "
                               << error_msg.toStdString();

    emit errorOccurred(QString("Failed to load change reason history: %1")
        .arg(error_msg));
    MessageBoxHelper::critical(this, "History Load Error",
        QString("Failed to load change reason history:\n%1")
        .arg(error_msg));
}

void ChangeReasonHistoryDialog::onVersionSelected(int index) {
    if (index < 0 || index >= static_cast<int>(versions_.size()))
        return;

    BOOST_LOG_SEV(lg(), trace) << "Version selected: " << index;

    displayChangesTab(index);
    displayFullDetailsTab(index);
}

void ChangeReasonHistoryDialog::displayChangesTab(int version_index) {
    ui_->changesTableWidget->setRowCount(0);

    if (version_index >= static_cast<int>(versions_.size()))
        return;

    const auto& current = versions_[version_index];

    // If this is the first (oldest) version, there's nothing to diff against so
    // leave the changes table empty
    if (version_index == static_cast<int>(versions_.size()) - 1)
        return;

    // Calculate diff with previous version
    const auto& previous = versions_[version_index + 1];
    auto diffs = calculateDiff(current, previous);

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

void ChangeReasonHistoryDialog::displayFullDetailsTab(int version_index) {
    if (version_index >= static_cast<int>(versions_.size()))
        return;

    const auto& reason = versions_[version_index];

    ui_->codeValue->setText(QString::fromStdString(reason.code));
    ui_->descriptionValue->setText(QString::fromStdString(reason.description));
    ui_->categoryCodeValue->setText(QString::fromStdString(reason.category_code));
    ui_->appliesToAmendValue->setText(reason.applies_to_amend ? "Yes" : "No");
    ui_->appliesToDeleteValue->setText(reason.applies_to_delete ? "Yes" : "No");
    ui_->requiresCommentaryValue->setText(reason.requires_commentary ? "Yes" : "No");
    ui_->displayOrderValue->setText(QString::number(reason.display_order));
    ui_->versionNumberValue->setText(QString::number(reason.version));
    ui_->recordedByValue->setText(QString::fromStdString(reason.recorded_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(reason.recorded_at));
    ui_->changeCommentaryValue->setText(QString::fromStdString(reason.change_commentary));
}

#define CHECK_DIFF_STRING(FIELD_NAME, FIELD) \
    if (current.FIELD != previous.FIELD) { \
        diffs.append({FIELD_NAME, { \
            QString::fromStdString(previous.FIELD), \
            QString::fromStdString(current.FIELD) \
        }}); \
    }

#define CHECK_DIFF_BOOL(FIELD_NAME, FIELD) \
    if (current.FIELD != previous.FIELD) { \
        diffs.append({FIELD_NAME, { \
            previous.FIELD ? "Yes" : "No", \
            current.FIELD ? "Yes" : "No" \
        }}); \
    }

#define CHECK_DIFF_INT(FIELD_NAME, FIELD) \
    if (current.FIELD != previous.FIELD) { \
        diffs.append({FIELD_NAME, { \
            QString::number(previous.FIELD), \
            QString::number(current.FIELD) \
        }}); \
    }

ChangeReasonHistoryDialog::DiffResult ChangeReasonHistoryDialog::
calculateDiff(const dq::domain::change_reason& current,
    const dq::domain::change_reason& previous) {

    DiffResult diffs;

    // Compare string fields
    CHECK_DIFF_STRING("Code", code);
    CHECK_DIFF_STRING("Description", description);
    CHECK_DIFF_STRING("Category Code", category_code);

    // Compare boolean fields
    CHECK_DIFF_BOOL("Applies to Amend", applies_to_amend);
    CHECK_DIFF_BOOL("Applies to Delete", applies_to_delete);
    CHECK_DIFF_BOOL("Requires Commentary", requires_commentary);

    // Compare integer fields
    CHECK_DIFF_INT("Display Order", display_order);

    return diffs;
}

#undef CHECK_DIFF_STRING
#undef CHECK_DIFF_BOOL
#undef CHECK_DIFF_INT

void ChangeReasonHistoryDialog::setupToolbar() {
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    // Create Reload action
    reloadAction_ = new QAction("Reload", this);
    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowClockwise, IconUtils::DefaultIconColor));
    reloadAction_->setToolTip("Reload history from server");
    connect(reloadAction_, &QAction::triggered, this,
        &ChangeReasonHistoryDialog::onReloadClicked);
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    // Create Open action
    openAction_ = new QAction("Open", this);
    openAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Edit, IconUtils::DefaultIconColor));
    openAction_->setToolTip("Open this version in read-only mode");
    connect(openAction_, &QAction::triggered, this,
        &ChangeReasonHistoryDialog::onOpenClicked);
    toolBar_->addAction(openAction_);

    // Create Revert action
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));
    revertAction_->setToolTip("Revert change reason to this version");
    connect(revertAction_, &QAction::triggered, this,
        &ChangeReasonHistoryDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);

    // Add toolbar to layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);
}

void ChangeReasonHistoryDialog::updateButtonStates() {
    const int index = selectedVersionIndex();
    const bool hasSelection = index >= 0 &&
        index < static_cast<int>(versions_.size());

    if (openAction_)
        openAction_->setEnabled(hasSelection);

    if (revertAction_)
        revertAction_->setEnabled(hasSelection);
}

int ChangeReasonHistoryDialog::selectedVersionIndex() const {
    return ui_->versionListWidget->currentRow();
}

void ChangeReasonHistoryDialog::onOpenClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(versions_.size()))
        return;

    const auto& version = versions_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening change reason version "
                              << version.version << " in read-only mode";

    emit openVersionRequested(version, version.version);
}

void ChangeReasonHistoryDialog::onRevertClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(versions_.size()))
        return;

    // Get the selected version and the previous (older) version
    const auto& current = versions_[index];

    // If this is the oldest version, there's no previous version to revert to
    if (index == static_cast<int>(versions_.size()) - 1) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot revert oldest version - no previous version exists";
        MessageBoxHelper::information(this, "Cannot Revert",
            "This is the oldest version. There is no previous version to revert to.");
        return;
    }

    // The "previous" version is the one we want to revert TO (the "old" side in the diff)
    const auto& previous = versions_[index + 1];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert from version "
                              << current.version << " to version "
                              << previous.version;

    // Confirm with user
    auto reply = MessageBoxHelper::question(this, "Revert Change Reason",
        QString("Are you sure you want to revert '%1' from version %2 back to version %3?\n\n"
                "This will create a new version with the data from version %3.")
            .arg(code_)
            .arg(current.version)
            .arg(previous.version),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Revert cancelled by user";
        return;
    }

    // Use the PREVIOUS version's data (the "old" side of the diff) with the latest version number
    dq::domain::change_reason reason = previous;
    reason.version = versions_[0].version;
    emit revertVersionRequested(reason);
}

void ChangeReasonHistoryDialog::onReloadClicked() {
    BOOST_LOG_SEV(lg(), info) << "Reload requested for change reason history: "
                              << code_.toStdString();
    emit statusChanged(QString("Reloading history for %1...").arg(code_));
    loadHistory();
}

QSize ChangeReasonHistoryDialog::sizeHint() const {
    // Call the base implementation first to get the minimum size required by
    // the layout manager and its content's size policies.
    QSize baseSize = QWidget::sizeHint();

    // Define a reasonable minimum size for a history dialog. This ensures the
    // two panes (Version List and Details/Changes) are comfortably visible.
    const int minimumWidth = 900;
    const int minimumHeight = 600;

    // Return the maximum of the base size (to accommodate large text/UI
    // elements) and the defined minimum size.
    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void ChangeReasonHistoryDialog::markAsStale() {
    BOOST_LOG_SEV(lg(), info) << "Change reason history marked as stale for: "
                              << code_.toStdString() << ", reloading...";

    emit statusChanged(QString("Change reason %1 was modified - reloading history...")
        .arg(code_));

    // Reload history data
    loadHistory();
}

}
