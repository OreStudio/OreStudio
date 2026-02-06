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
#include "ores.qt/TenantHistoryDialog.hpp"

#include <QIcon>
#include <QDateTime>
#include <QScrollBar>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_TenantHistoryDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.comms/net/client_session.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

struct HistoryResult {
    bool success;
    std::vector<iam::domain::tenant> versions;
    QString error_message;
    QString error_details;
};

}

const QIcon& TenantHistoryDialog::getHistoryIcon() const {
    static const QIcon historyIcon = IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor);
    return historyIcon;
}

TenantHistoryDialog::TenantHistoryDialog(const QString& tenantCode,
    ClientManager* clientManager, QWidget* parent)
    : QWidget(parent), ui_(new Ui::TenantHistoryDialog),
      clientManager_(clientManager), tenantCode_(tenantCode),
      toolBar_(nullptr), reloadAction_(nullptr),
      openAction_(nullptr), revertAction_(nullptr) {

    BOOST_LOG_SEV(lg(), info) << "Creating tenant history widget for: "
                              << tenantCode_.toStdString();

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

TenantHistoryDialog::~TenantHistoryDialog() {
    BOOST_LOG_SEV(lg(), info) << "Destroying tenant history widget";

    // Disconnect and cancel any active QFutureWatcher objects
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void TenantHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading tenant history for: "
                              << tenantCode_.toStdString();

    if (!clientManager_ || !clientManager_->isConnected()) {
        onHistoryLoadError("Not connected to server");
        return;
    }

    QPointer<TenantHistoryDialog> self = this;

    // First we need to get the tenant ID from the code
    // For now, we'll need to find the tenant by code in the list
    // The history request requires a UUID

    QFuture<HistoryResult> future = QtConcurrent::run([self]() -> HistoryResult {
        return exception_helper::wrap_async_fetch<HistoryResult>([&]() -> HistoryResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .versions = {},
                        .error_message = "Dialog closed",
                        .error_details = {}};
            }

            // First get all tenants to find the ID for this code
            iam::messaging::get_tenants_request tenants_request;
            tenants_request.include_deleted = true;  // Include historical versions

            auto tenants_result = self->clientManager_->
                process_authenticated_request(std::move(tenants_request));

            if (!tenants_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to fetch tenants: "
                                           << comms::net::to_string(tenants_result.error());
                return {.success = false, .versions = {},
                        .error_message = QString::fromStdString(
                            "Failed to fetch tenants: " +
                            comms::net::to_string(tenants_result.error())),
                        .error_details = {}};
            }

            // Find the tenant with matching code
            boost::uuids::uuid tenant_id;
            bool found = false;
            for (const auto& tenant : tenants_result->tenants) {
                if (tenant.code == self->tenantCode_.toStdString()) {
                    tenant_id = tenant.id;
                    found = true;
                    break;
                }
            }

            if (!found) {
                return {.success = false, .versions = {},
                        .error_message = QString("Tenant with code '%1' not found")
                            .arg(self->tenantCode_),
                        .error_details = {}};
            }

            // Now get the history
            iam::messaging::get_tenant_history_request history_request;
            history_request.id = tenant_id;

            auto history_result = self->clientManager_->
                process_authenticated_request(std::move(history_request));

            if (!history_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to fetch tenant history: "
                                           << comms::net::to_string(history_result.error());
                return {.success = false, .versions = {},
                        .error_message = QString::fromStdString(
                            "Failed to fetch tenant history: " +
                            comms::net::to_string(history_result.error())),
                        .error_details = {}};
            }

            if (!history_result->success) {
                return {.success = false, .versions = {},
                        .error_message = QString::fromStdString(history_result->message),
                        .error_details = {}};
            }

            return {.success = true, .versions = std::move(history_result->versions),
                    .error_message = {}, .error_details = {}};
        }, "tenant history");
    });

    auto* watcher = new QFutureWatcher<HistoryResult>(self);
    connect(watcher, &QFutureWatcher<HistoryResult>::finished, self,
        [self, watcher]() {
        if (!self) return;

        const auto result = watcher->result();
        watcher->deleteLater();

        if (!result.success) {
            self->onHistoryLoadError(result.error_message);
            return;
        }

        self->history_ = std::move(result.versions);
        self->onHistoryLoaded();
    });

    watcher->setFuture(future);
}

void TenantHistoryDialog::onHistoryLoaded() {
    BOOST_LOG_SEV(lg(), info) << "History loaded successfully: "
                              << history_.size() << " versions";

    const QIcon& cachedIcon = getHistoryIcon();
    ui_->versionListWidget->setRowCount(0);
    ui_->versionListWidget->setRowCount(static_cast<int>(history_.size()));

    for (int i = 0; i < static_cast<int>(history_.size()); ++i) {
        const auto& version = history_[i];

        auto* versionItem =
            new QTableWidgetItem(QString::number(version.version));
        auto* recordedAtItem =
            new QTableWidgetItem(relative_time_helper::format(version.recorded_at));
        auto* recordedByItem =
            new QTableWidgetItem(QString::fromStdString(version.recorded_by));

        versionItem->setIcon(cachedIcon);

        ui_->versionListWidget->setItem(i, 0, versionItem);
        ui_->versionListWidget->setItem(i, 1, recordedAtItem);
        ui_->versionListWidget->setItem(i, 2, recordedByItem);
    }

    if (!history_.empty())
        ui_->versionListWidget->selectRow(0);

    if (!history_.empty()) {
        const auto& latest = history_[0];
        ui_->titleLabel->setText(QString("Tenant History: %1")
            .arg(QString::fromStdString(latest.code)));
    }

    updateButtonStates();

    emit statusChanged(QString("Loaded %1 versions")
        .arg(history_.size()));
}

void TenantHistoryDialog::onHistoryLoadError(const QString& error_msg) {
    BOOST_LOG_SEV(lg(), error) << "Error loading history: "
                               << error_msg.toStdString();

    emit errorOccurred(QString("Failed to load tenant history: %1")
        .arg(error_msg));
    MessageBoxHelper::critical(this, "History Load Error",
        QString("Failed to load tenant history:\n%1")
        .arg(error_msg));
}

void TenantHistoryDialog::onVersionSelected(int index) {
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    BOOST_LOG_SEV(lg(), trace) << "Version selected: " << index;

    displayChangesTab(index);
    displayFullDetailsTab(index);
}

void TenantHistoryDialog::displayChangesTab(int version_index) {
    ui_->changesTableWidget->setRowCount(0);

    if (version_index >= static_cast<int>(history_.size()))
        return;

    const auto& current = history_[version_index];

    // If this is the first (oldest) version, there's nothing to diff against
    if (version_index == static_cast<int>(history_.size()) - 1)
        return;

    // Calculate diff with previous version
    const auto& previous = history_[version_index + 1];
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

void TenantHistoryDialog::displayFullDetailsTab(int version_index) {
    if (version_index >= static_cast<int>(history_.size()))
        return;

    const auto& tenant = history_[version_index];

    ui_->codeValue->setText(QString::fromStdString(tenant.code));
    ui_->nameValue->setText(QString::fromStdString(tenant.name));
    ui_->typeValue->setText(QString::fromStdString(tenant.type));
    ui_->hostnameValue->setText(QString::fromStdString(tenant.hostname));
    ui_->statusValue->setText(QString::fromStdString(tenant.status));
    ui_->descriptionValue->setText(QString::fromStdString(tenant.description));
    ui_->versionNumberValue->setText(QString::number(tenant.version));
    ui_->recordedByValue->setText(QString::fromStdString(tenant.recorded_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(tenant.recorded_at));
}

#define CHECK_DIFF_STRING(FIELD_NAME, FIELD) \
    if (current.FIELD != previous.FIELD) { \
        diffs.append({FIELD_NAME, { \
            QString::fromStdString(previous.FIELD), \
            QString::fromStdString(current.FIELD) \
        }}); \
    }

TenantHistoryDialog::DiffResult TenantHistoryDialog::
calculateDiff(const iam::domain::tenant& current,
    const iam::domain::tenant& previous) {

    DiffResult diffs;

    CHECK_DIFF_STRING("Code", code);
    CHECK_DIFF_STRING("Name", name);
    CHECK_DIFF_STRING("Type", type);
    CHECK_DIFF_STRING("Hostname", hostname);
    CHECK_DIFF_STRING("Status", status);
    CHECK_DIFF_STRING("Description", description);

    return diffs;
}

#undef CHECK_DIFF_STRING

void TenantHistoryDialog::setupToolbar() {
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    // Create Reload action
    reloadAction_ = new QAction("Reload", this);
    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowClockwise, IconUtils::DefaultIconColor));
    reloadAction_->setToolTip("Reload history from server");
    connect(reloadAction_, &QAction::triggered, this,
        &TenantHistoryDialog::onReloadClicked);
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    // Create Open action
    openAction_ = new QAction("Open", this);
    openAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Edit, IconUtils::DefaultIconColor));
    openAction_->setToolTip("Open this version in read-only mode");
    connect(openAction_, &QAction::triggered, this,
        &TenantHistoryDialog::onOpenClicked);
    toolBar_->addAction(openAction_);

    // Create Revert action
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));
    revertAction_->setToolTip("Revert tenant to this version");
    connect(revertAction_, &QAction::triggered, this,
        &TenantHistoryDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);

    // Add toolbar to layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);
}

void TenantHistoryDialog::updateButtonStates() {
    const int index = selectedVersionIndex();
    const bool hasSelection = index >= 0 &&
        index < static_cast<int>(history_.size());

    if (openAction_)
        openAction_->setEnabled(hasSelection);

    if (revertAction_)
        revertAction_->setEnabled(hasSelection);
}

int TenantHistoryDialog::selectedVersionIndex() const {
    return ui_->versionListWidget->currentRow();
}

void TenantHistoryDialog::onOpenClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    const auto& tenant = history_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening tenant version "
                              << tenant.version << " in read-only mode";

    emit openVersionRequested(tenant, tenant.version);
}

void TenantHistoryDialog::onRevertClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    const auto& current = history_[index];

    // If this is the oldest version, there's no previous version to revert to
    if (index == static_cast<int>(history_.size()) - 1) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot revert oldest version - no previous version exists";
        MessageBoxHelper::information(this, "Cannot Revert",
            "This is the oldest version. There is no previous version to revert to.");
        return;
    }

    // The "previous" version is the one we want to revert TO
    const auto& previous = history_[index + 1];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert from version "
                              << current.version << " to version "
                              << previous.version;

    // Confirm with user
    auto reply = MessageBoxHelper::question(this, "Revert Tenant",
        QString("Are you sure you want to revert '%1' from version %2 back to version %3?\n\n"
                "This will create a new version with the data from version %3.")
            .arg(tenantCode_)
            .arg(current.version)
            .arg(previous.version),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Revert cancelled by user";
        return;
    }

    // Use the PREVIOUS version's data with the latest version number
    iam::domain::tenant tenant = previous;
    tenant.version = history_[0].version;
    emit revertVersionRequested(tenant);
}

void TenantHistoryDialog::onReloadClicked() {
    BOOST_LOG_SEV(lg(), info) << "Reload requested for tenant history: "
                              << tenantCode_.toStdString();
    emit statusChanged(QString("Reloading history for %1...").arg(tenantCode_));
    loadHistory();
}

QSize TenantHistoryDialog::sizeHint() const {
    QSize baseSize = QWidget::sizeHint();

    const int minimumWidth = 900;
    const int minimumHeight = 600;

    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void TenantHistoryDialog::markAsStale() {
    BOOST_LOG_SEV(lg(), info) << "Tenant history marked as stale for: "
                              << tenantCode_.toStdString() << ", reloading...";

    emit statusChanged(QString("Tenant %1 was modified - reloading history...")
        .arg(tenantCode_));

    loadHistory();
}

}
