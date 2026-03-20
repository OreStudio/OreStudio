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
#include "ores.qt/SystemSettingHistoryDialog.hpp"

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
#include "ores.qt/WidgetUtils.hpp"
#include "ores.variability/messaging/system_settings_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

const QIcon& SystemSettingHistoryDialog::getHistoryIcon() const {
    static const QIcon historyIcon = IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor);
    return historyIcon;
}

SystemSettingHistoryDialog::SystemSettingHistoryDialog(QString name,
    ClientManager* clientManager, QWidget* parent)
    : QWidget(parent), ui_(new Ui::SystemSettingHistoryDialog),
      clientManager_(clientManager),
      flagName_(std::move(name)),
      toolBar_(nullptr), reloadAction_(nullptr),
      openAction_(nullptr), revertAction_(nullptr) {

    BOOST_LOG_SEV(lg(), info) << "Creating system setting history widget for: "
                              << flagName_.toStdString();

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

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

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
    connect(ui_->closeButton, &QPushButton::clicked,
            this, [this]() { if (window()) window()->close(); });

    updateButtonStates();
}

SystemSettingHistoryDialog::~SystemSettingHistoryDialog() {
    BOOST_LOG_SEV(lg(), info) << "Destroying system setting history widget";

    // Disconnect and cancel any active QFutureWatcher objects
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void SystemSettingHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading system setting history for: "
                              << flagName_.toStdString();

    using HistoryResult = std::expected<variability::messaging::get_setting_history_response, std::string>;
    QPointer<SystemSettingHistoryDialog> self = this;
    const auto flagName = flagName_.toStdString();

    QFuture<HistoryResult> future =
        QtConcurrent::run([self, flagName]() -> HistoryResult {
        if (!self->clientManager_ || !self->clientManager_->isConnected()) {
            return std::unexpected("Disconnected from server");
        }
        variability::messaging::get_setting_history_request request;
        request.name = flagName;
        auto result = self->clientManager_->process_authenticated_request(std::move(request));
        if (!result) {
            return std::unexpected(result.error());
        }
        return std::move(*result);
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

        if (!result->success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            self->onHistoryLoadError(QString::fromStdString(result->message));
            return;
        }

        self->history_ = std::move(result->history);
        self->onHistoryLoaded();
    });

    watcher->setFuture(future);
}

void SystemSettingHistoryDialog::onHistoryLoaded() {
    BOOST_LOG_SEV(lg(), info) << "History loaded successfully: "
                              << history_.size() << " versions";

    const QIcon& cachedIcon = getHistoryIcon();
    ui_->versionListWidget->setRowCount(0);
    ui_->versionListWidget->setRowCount(history_.size());

    for (int i = 0; i < static_cast<int>(history_.size()); ++i) {
        const auto& flag = history_[i];

        BOOST_LOG_SEV(lg(), trace) << "Displaying version [" << i << "]: "
                                   << "version=" << flag.version
                                   << ", modified_by=" << flag.modified_by;

        auto* versionItem =
            new QTableWidgetItem(QString::number(flag.version));
        auto* enabledItem =
            new QTableWidgetItem((flag.value == "true") ? "Yes" : "No");
        auto* recordedAtItem =
            new QTableWidgetItem(relative_time_helper::format(flag.recorded_at));
        auto* modifiedByItem =
            new QTableWidgetItem(QString::fromStdString(flag.modified_by));

        versionItem->setIcon(cachedIcon);

        ui_->versionListWidget->setItem(i, 0, versionItem);
        ui_->versionListWidget->setItem(i, 1, enabledItem);
        ui_->versionListWidget->setItem(i, 2, recordedAtItem);
        ui_->versionListWidget->setItem(i, 3, modifiedByItem);
    }

    if (!history_.empty())
        ui_->versionListWidget->selectRow(0);

    if (!history_.empty()) {
        const auto& latest = history_[0];
        ui_->titleLabel->setText(QString("System Setting History: %1")
            .arg(QString::fromStdString(latest.name)));
    }

    updateButtonStates();

    emit statusChanged(QString("Loaded %1 versions")
        .arg(history_.size()));
}

void SystemSettingHistoryDialog::onHistoryLoadError(const QString& error_msg) {
    BOOST_LOG_SEV(lg(), error) << "Error loading history: "
                               << error_msg.toStdString();

    emit errorOccurred(QString("Failed to load system setting history: %1")
        .arg(error_msg));
    MessageBoxHelper::critical(this, "History Load Error",
        QString("Failed to load system setting history:\n%1")
        .arg(error_msg));
}

void SystemSettingHistoryDialog::onVersionSelected(int index) {
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    BOOST_LOG_SEV(lg(), trace) << "Version selected: " << index;

    displayChangesTab(index);
    displayFullDetailsTab(index);
}

void SystemSettingHistoryDialog::displayChangesTab(int version_index) {
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

void SystemSettingHistoryDialog::displayFullDetailsTab(int version_index) {
    if (version_index >= static_cast<int>(history_.size()))
        return;

    const auto& flag = history_[version_index];

    ui_->nameValue->setText(QString::fromStdString(flag.name));
    ui_->enabledValue->setText((flag.value == "true") ? "Yes" : "No");
    ui_->descriptionValue->setText(QString::fromStdString(flag.description));
    ui_->versionNumberValue->setText(QString::number(flag.version));
    ui_->modifiedByValue->setText(QString::fromStdString(flag.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(flag.recorded_at));
}

SystemSettingHistoryDialog::DiffResult SystemSettingHistoryDialog::
calculateDiff(const variability::domain::system_setting& current,
    const variability::domain::system_setting& previous) {

    DiffResult diffs;

    // Compare value (enabled flag)
    if (current.value != previous.value) {
        diffs.append({"Enabled", {
            (previous.value == "true") ? "Yes" : "No",
            (current.value == "true") ? "Yes" : "No"
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

void SystemSettingHistoryDialog::setupToolbar() {
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    // Create Reload action
    reloadAction_ = new QAction("Reload", this);
    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowClockwise, IconUtils::DefaultIconColor));
    reloadAction_->setToolTip("Reload history from server");
    connect(reloadAction_, &QAction::triggered, this,
        &SystemSettingHistoryDialog::onReloadClicked);
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    // Create Open action
    openAction_ = new QAction("Open", this);
    openAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Edit, IconUtils::DefaultIconColor));
    openAction_->setToolTip("Open this version in read-only mode");
    connect(openAction_, &QAction::triggered, this,
        &SystemSettingHistoryDialog::onOpenClicked);
    toolBar_->addAction(openAction_);

    // Create Revert action
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));
    revertAction_->setToolTip("Revert system setting to this version");
    connect(revertAction_, &QAction::triggered, this,
        &SystemSettingHistoryDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);

    // Add toolbar to layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);
}

void SystemSettingHistoryDialog::updateButtonStates() {
    const int index = selectedVersionIndex();
    const bool hasSelection = index >= 0 &&
        index < static_cast<int>(history_.size());

    if (openAction_)
        openAction_->setEnabled(hasSelection);

    if (revertAction_)
        revertAction_->setEnabled(hasSelection);
}

int SystemSettingHistoryDialog::selectedVersionIndex() const {
    return ui_->versionListWidget->currentRow();
}

void SystemSettingHistoryDialog::onOpenClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    const auto& flag = history_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening system setting version "
                              << flag.version << " in read-only mode";

    emit openVersionRequested(flag, flag.version);
}

void SystemSettingHistoryDialog::onRevertClicked() {
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
    auto reply = MessageBoxHelper::question(this, "Revert System Setting",
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
    variability::domain::system_setting flagToRevert = previous;
    emit revertVersionRequested(flagToRevert);
}

void SystemSettingHistoryDialog::onReloadClicked() {
    BOOST_LOG_SEV(lg(), info) << "Reload requested for system setting history: "
                              << flagName_.toStdString();
    emit statusChanged(QString("Reloading history for %1...").arg(flagName_));
    loadHistory();
}

QSize SystemSettingHistoryDialog::sizeHint() const {
    QSize baseSize = QWidget::sizeHint();

    const int minimumWidth = 800;
    const int minimumHeight = 500;

    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void SystemSettingHistoryDialog::markAsStale() {
    BOOST_LOG_SEV(lg(), info) << "System setting history marked as stale for: "
                              << flagName_.toStdString() << ", reloading...";

    emit statusChanged(QString("System setting %1 was modified - reloading history...")
        .arg(flagName_));

    // Reload history data
    loadHistory();
}

}
