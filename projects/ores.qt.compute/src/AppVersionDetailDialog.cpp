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
#include "ores.qt/AppVersionDetailDialog.hpp"

#include <QFile>
#include <QFileInfo>
#include <QHeaderView>
#include <QMessageBox>
#include <QFileDialog>
#include <QProgressBar>
#include <QPushButton>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <QListWidgetItem>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QNetworkAccessManager>
#include <QTableWidgetItem>
#include <memory>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ui_AppVersionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.compute.api/messaging/app_protocol.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.compute.api/messaging/platform_protocol.hpp"
#include "ores.compute.api/net/compute_storage.hpp"

namespace ores::qt {

using namespace ores::logging;

AppVersionDetailDialog::AppVersionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::AppVersionDetailDialog),
      clientManager_(nullptr),
      // Single QNetworkAccessManager shared across all per-row PUTs so we
      // reuse its connection pool / HTTP keep-alive instead of standing up
      // a fresh manager per upload. Parented to the dialog for cleanup.
      networkManager_(new QNetworkAccessManager(this)) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

AppVersionDetailDialog::~AppVersionDetailDialog() {
    delete ui_;
}

QTabWidget* AppVersionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* AppVersionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* AppVersionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void AppVersionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    ui_->packagesTable->setColumnWidth(0, 200);
    ui_->packagesTable->setColumnWidth(1, 220);
    ui_->packagesTable->setColumnWidth(2, 100);
    ui_->packagesTable->horizontalHeader()->setStretchLastSection(false);
}

void AppVersionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onCloseClicked);

    connect(ui_->addPlatformButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onAddPlatformClicked);
    connect(ui_->removePlatformButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onRemovePlatformClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &AppVersionDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &AppVersionDetailDialog::onFieldChanged);
    connect(ui_->assignedPlatformsList, &QListWidget::itemSelectionChanged, this,
            &AppVersionDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &AppVersionDetailDialog::onFieldChanged);
    connect(ui_->appCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
            &AppVersionDetailDialog::onFieldChanged);
}

void AppVersionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    loadApps();
    loadPlatforms();
}

void AppVersionDetailDialog::loadApps() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<AppVersionDetailDialog> self = this;

    struct FetchResult {
        bool success;
        std::vector<AppEntry> entries;
        std::string error;
    };

    auto task = [self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {false, {}, "Dialog closed"};

        compute::messaging::list_apps_request request;
        request.limit = 1000;
        auto result = self->clientManager_->process_authenticated_request(
            std::move(request));

        if (!result)
            return {false, {}, "Failed to fetch apps"};

        std::vector<AppEntry> entries;
        entries.reserve(result->apps.size());
        for (const auto& app : result->apps) {
            entries.push_back({boost::uuids::to_string(app.id), app.name});
        }
        return {true, std::move(entries), {}};
    };

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    connect(watcher, &QFutureWatcher<FetchResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (self && result.success) {
            self->appEntries_ = std::move(result.entries);
            self->populateAppCombo();
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void AppVersionDetailDialog::loadPlatforms() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<AppVersionDetailDialog> self = this;

    struct FetchResult {
        bool success;
        std::vector<PlatformEntry> entries;
    };

    auto task = [self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {false, {}};

        compute::messaging::list_platforms_request request;
        auto result = self->clientManager_->process_authenticated_request(
            std::move(request));

        if (!result) return {false, {}};

        std::vector<PlatformEntry> entries;
        entries.reserve(result->platforms.size());
        for (const auto& p : result->platforms) {
            entries.push_back({
                boost::uuids::to_string(p.id),
                p.code,
                p.display_name
            });
        }
        return {true, std::move(entries)};
    };

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    connect(watcher, &QFutureWatcher<FetchResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (self && result.success) {
            self->availablePlatforms_ = std::move(result.entries);
            self->populatePlatformsTab();
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void AppVersionDetailDialog::populateAppCombo() {
    ui_->appCombo->blockSignals(true);
    ui_->appCombo->clear();
    ui_->appCombo->addItem(tr("(select app)"), QString());  // sentinel
    for (const auto& entry : appEntries_) {
        ui_->appCombo->addItem(
            QString::fromStdString(entry.name),
            QString::fromStdString(entry.id));
    }
    // Re-apply current app_id selection if already set
    if (!app_version_.app_id.is_nil()) {
        const QString idStr = QString::fromStdString(
            boost::uuids::to_string(app_version_.app_id));
        for (int i = 0; i < ui_->appCombo->count(); ++i) {
            if (ui_->appCombo->itemData(i).toString() == idStr) {
                ui_->appCombo->setCurrentIndex(i);
                break;
            }
        }
    }
    ui_->appCombo->blockSignals(false);
}

void AppVersionDetailDialog::populatePlatformsTab() {
    ui_->availablePlatformsList->clear();
    ui_->assignedPlatformsList->clear();

    for (const auto& p : availablePlatforms_) {
        const bool assigned = std::find_if(package_rows_.begin(),
            package_rows_.end(), [&](const auto& row) {
                return row.platform_id == p.id;
            }) != package_rows_.end();

        auto* item = new QListWidgetItem(QString::fromStdString(p.display_name));
        item->setData(Qt::UserRole, QString::fromStdString(p.id));

        if (assigned)
            ui_->assignedPlatformsList->addItem(item);
        else
            ui_->availablePlatformsList->addItem(item);
    }
    syncPackagesTab();
}

void AppVersionDetailDialog::syncPackagesTab() {
    // Rebuild package_rows_ to mirror the assigned list, preserving existing
    // state (uploaded URIs, selected files) for platforms still present.
    std::vector<PackageRow> rebuilt;
    rebuilt.reserve(ui_->assignedPlatformsList->count());
    for (int i = 0; i < ui_->assignedPlatformsList->count(); ++i) {
        const auto* item = ui_->assignedPlatformsList->item(i);
        if (!item) continue;
        const auto pid = item->data(Qt::UserRole).toString().toStdString();
        const auto avail = std::find_if(availablePlatforms_.begin(),
            availablePlatforms_.end(),
            [&](const auto& p) { return p.id == pid; });
        if (avail == availablePlatforms_.end()) continue;

        PackageRow row;
        row.platform_id = avail->id;
        row.platform_code = avail->code;
        row.platform_name = avail->display_name;

        // Preserve state from the previous rows_ if we already tracked this
        // platform — keeps selected files / upload status through add/remove
        // churn and across the async junction-fetch completion.
        const auto prev = std::find_if(package_rows_.begin(),
            package_rows_.end(),
            [&](const auto& r) { return r.platform_id == pid; });
        if (prev != package_rows_.end()) {
            row.local_file = prev->local_file;
            row.remote_uri = prev->remote_uri;
            row.state = prev->state;
            row.error = prev->error;
        }
        rebuilt.push_back(std::move(row));
    }
    package_rows_ = std::move(rebuilt);

    ui_->packagesTable->setRowCount(static_cast<int>(package_rows_.size()));
    for (int i = 0; i < static_cast<int>(package_rows_.size()); ++i)
        updatePackagesTableRow(i);
}

void AppVersionDetailDialog::updatePackagesTableRow(int row) {
    if (row < 0 || row >= static_cast<int>(package_rows_.size())) return;
    const auto& pr = package_rows_[row];

    auto* name_item = new QTableWidgetItem(
        QString::fromStdString(pr.platform_name.empty()
            ? pr.platform_code : pr.platform_name));
    name_item->setToolTip(QString::fromStdString(pr.platform_code));
    ui_->packagesTable->setItem(row, 0, name_item);

    QString file_text;
    if (!pr.local_file.isEmpty())
        file_text = QFileInfo(pr.local_file).fileName();
    else if (!pr.remote_uri.isEmpty())
        file_text = tr("(on server)");
    else
        file_text = tr("— no file —");
    auto* file_item = new QTableWidgetItem(file_text);
    file_item->setToolTip(pr.local_file.isEmpty()
        ? pr.remote_uri : pr.local_file);
    ui_->packagesTable->setItem(row, 1, file_item);

    QString status;
    switch (pr.state) {
    case PackageRow::State::NoFile:    status = tr("No file");  break;
    case PackageRow::State::Selected:  status = tr("Selected"); break;
    case PackageRow::State::Uploading: status = tr("Uploading…"); break;
    case PackageRow::State::Uploaded:  status = tr("Uploaded"); break;
    case PackageRow::State::Failed:    status = tr("Failed");   break;
    }
    auto* status_item = new QTableWidgetItem(status);
    if (!pr.error.isEmpty()) status_item->setToolTip(pr.error);
    ui_->packagesTable->setItem(row, 2, status_item);

    // Dedicated cell widget so we can attach a row-scoped Browse button with
    // a dynamic property pointing at our row index; a simple onClick mapper
    // keeps state consistent when table rows are rebuilt.
    auto* btn = new QPushButton(tr("Browse…"), ui_->packagesTable);
    btn->setProperty("package_row", row);
    btn->setEnabled(!readOnly_ && !saveInProgress_);
    connect(btn, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onBrowsePackageRowClicked);
    ui_->packagesTable->setCellWidget(row, 3, btn);
}

void AppVersionDetailDialog::setPackageRowState(int row,
    PackageRow::State state, const QString& error) {
    if (row < 0 || row >= static_cast<int>(package_rows_.size())) return;
    package_rows_[row].state = state;
    package_rows_[row].error = error;
    updatePackagesTableRow(row);
}

void AppVersionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void AppVersionDetailDialog::setHttpBaseUrl(const std::string& url) {
    httpBaseUrl_ = QUrl(QString::fromStdString(url));
}

void AppVersionDetailDialog::setVersion(
    const compute::domain::app_version& app_version) {
    app_version_ = app_version;
    // Start from an empty set; populatePlatformsTab will re-run once the
    // junction fetch returns. In create mode there is nothing to load
    // because the id has just been generated.
    package_rows_.clear();
    updateUiFromVersion();

    if (!createMode_ && !app_version_.id.is_nil())
        loadAssignedPlatforms(boost::uuids::to_string(app_version_.id));
}

void AppVersionDetailDialog::loadAssignedPlatforms(
    const std::string& app_version_id) {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<AppVersionDetailDialog> self = this;

    struct FetchResult {
        bool success;
        std::vector<compute::domain::app_version_platform> rows;
    };

    auto task = [self, app_version_id]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {false, {}};

        compute::messaging::list_app_version_platforms_request request;
        request.app_version_id = app_version_id;
        auto result = self->clientManager_->process_authenticated_request(
            std::move(request));

        if (!result || !result->success) return {false, {}};
        return {true, std::move(result->platforms)};
    };

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    connect(watcher, &QFutureWatcher<FetchResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self || !result.success) return;

        // Seed package_rows_ from the junction so populatePlatformsTab sees
        // these platforms as already-assigned. Every loaded row starts as
        // Uploaded — its URI is whatever the server handed back, and the
        // user only has to re-upload if they explicitly pick a new file.
        self->package_rows_.clear();
        self->package_rows_.reserve(result.rows.size());
        for (const auto& r : result.rows) {
            PackageRow pr;
            pr.platform_id = boost::uuids::to_string(r.platform_id);
            pr.platform_code = r.platform_code;
            pr.remote_uri = QString::fromStdString(r.package_uri);
            pr.state = PackageRow::State::Uploaded;
            self->package_rows_.push_back(std::move(pr));
        }
        self->populatePlatformsTab();
        // populatePlatformsTab flips hasChanges_ via selection-changed; reset
        // because this was a server-driven refresh, not a user edit.
        self->hasChanges_ = false;
        self->updateSaveButtonState();
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void AppVersionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        app_version_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void AppVersionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->appCombo->setEnabled(!readOnly);
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->addPlatformButton->setEnabled(!readOnly);
    ui_->removePlatformButton->setEnabled(!readOnly);
    ui_->assignedPlatformsList->setEnabled(!readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->minRamSpinBox->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
    // Re-emit row updates so the per-row Browse buttons pick up the new
    // enabled state.
    for (int i = 0; i < static_cast<int>(package_rows_.size()); ++i)
        updatePackagesTableRow(i);
}

void AppVersionDetailDialog::updateUiFromVersion() {
    // Select the app in the combo (if apps are already loaded)
    if (!app_version_.app_id.is_nil()) {
        const QString idStr = QString::fromStdString(
            boost::uuids::to_string(app_version_.app_id));
        for (int i = 0; i < ui_->appCombo->count(); ++i) {
            if (ui_->appCombo->itemData(i).toString() == idStr) {
                ui_->appCombo->setCurrentIndex(i);
                break;
            }
        }
    }

    ui_->codeEdit->setText(QString::fromStdString(app_version_.wrapper_version));
    ui_->nameEdit->setText(QString::fromStdString(app_version_.engine_version));
    populatePlatformsTab();
    ui_->minRamSpinBox->setValue(app_version_.min_ram_mb);
    ui_->descriptionEdit->setPlainText({});

    populateProvenance(app_version_.version,
                       app_version_.modified_by,
                       app_version_.performed_by,
                       app_version_.recorded_at,
                       app_version_.change_reason_code,
                       app_version_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void AppVersionDetailDialog::updateVersionFromUi() {
    // Resolve selected app UUID
    const QString appIdStr = ui_->appCombo->currentData().toString();
    if (!appIdStr.isEmpty()) {
        try {
            app_version_.app_id = boost::lexical_cast<boost::uuids::uuid>(
                appIdStr.toStdString());
        } catch (...) {}
    }

    if (createMode_) {
        app_version_.wrapper_version = ui_->codeEdit->text().trimmed().toStdString();
    }
    app_version_.engine_version = ui_->nameEdit->text().trimmed().toStdString();
    app_version_.min_ram_mb = ui_->minRamSpinBox->value();
    app_version_.modified_by = username_;
    app_version_.performed_by = username_;
}

void AppVersionDetailDialog::onAddPlatformClicked() {
    const auto selected = ui_->availablePlatformsList->selectedItems();
    if (selected.isEmpty())
        return;

    for (auto* item : selected) {
        ui_->availablePlatformsList->takeItem(
            ui_->availablePlatformsList->row(item));
        ui_->assignedPlatformsList->addItem(item);
    }
    syncPackagesTab();
    hasChanges_ = true;
    updateSaveButtonState();
}

void AppVersionDetailDialog::onRemovePlatformClicked() {
    const auto selected = ui_->assignedPlatformsList->selectedItems();
    if (selected.isEmpty())
        return;

    for (auto* item : selected) {
        ui_->assignedPlatformsList->takeItem(
            ui_->assignedPlatformsList->row(item));
        ui_->availablePlatformsList->addItem(item);
    }
    syncPackagesTab();
    hasChanges_ = true;
    updateSaveButtonState();
}

void AppVersionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void AppVersionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void AppVersionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool AppVersionDetailDialog::validateInput() {
    const QString wrapper_version = ui_->codeEdit->text().trimmed();
    const QString engine_version = ui_->nameEdit->text().trimmed();
    const QString app_id = ui_->appCombo->currentData().toString();
    if (wrapper_version.isEmpty() || engine_version.isEmpty() || app_id.isEmpty())
        return false;

    // Require at least one platform assigned and a file (or already-uploaded
    // URI) for every assigned row — otherwise save would write junction rows
    // pointing at blobs that don't exist.
    if (package_rows_.empty()) return false;
    for (const auto& r : package_rows_) {
        if (r.local_file.isEmpty() && r.remote_uri.isEmpty())
            return false;
    }
    return true;
}

void AppVersionDetailDialog::onBrowsePackageRowClicked() {
    auto* btn = qobject_cast<QPushButton*>(sender());
    if (!btn) return;
    const int row = btn->property("package_row").toInt();
    if (row < 0 || row >= static_cast<int>(package_rows_.size())) return;

    const QString path = QFileDialog::getOpenFileName(
        this, tr("Select Package for %1").arg(
            QString::fromStdString(package_rows_[row].platform_code)),
        QString(),
        tr("Package Files (*.tar.gz);;All Files (*)"));
    if (path.isEmpty()) return;

    package_rows_[row].local_file = path;
    package_rows_[row].state = PackageRow::State::Selected;
    package_rows_[row].error.clear();
    updatePackagesTableRow(row);

    hasChanges_ = true;
    updateSaveButtonState();
}

void AppVersionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save app version while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields and select a package file "
            "for every assigned platform.");
        return;
    }

    updateVersionFromUi();

    const auto crOpType = createMode_
        ? ChangeReasonDialog::OperationType::Create
        : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_,
        createMode_ ? "system" : "common");
    if (!crSel) return;
    app_version_.change_reason_code = crSel->reason_code;
    app_version_.change_commentary = crSel->commentary;

    BOOST_LOG_SEV(lg(), info) << "Saving app version: " << app_version_.wrapper_version;

    QPointer<AppVersionDetailDialog> self = this;
    saveInProgress_ = true;
    ui_->saveButton->setEnabled(false);
    // Freeze the assigned-platform list while uploads are in flight. If the
    // user were allowed to add/remove rows mid-save, syncPackagesTab would
    // rebuild package_rows_ and the callbacks below (which find rows by
    // platform_id) could end up racing against the new state.
    ui_->addPlatformButton->setEnabled(false);
    ui_->removePlatformButton->setEnabled(false);
    ui_->packagesProgressBar->setVisible(true);
    ui_->packagesProgressBar->setValue(0);

    uploadPendingPackages([self](bool ok, QString err) {
        if (!self) return;
        self->ui_->packagesProgressBar->setVisible(false);
        self->saveInProgress_ = false;
        if (!self->readOnly_) {
            self->ui_->addPlatformButton->setEnabled(true);
            self->ui_->removePlatformButton->setEnabled(true);
        }
        if (!ok) {
            BOOST_LOG_SEV(lg(), error) << "Package upload failed: "
                                       << err.toStdString();
            MessageBoxHelper::critical(self, tr("Upload Failed"),
                tr("One or more packages could not be uploaded:\n%1").arg(err));
            self->updateSaveButtonState();
            return;
        }
        self->submitSave();
    });
}

void AppVersionDetailDialog::uploadPendingPackages(
    std::function<void(bool, QString)> done) {
    // Collect rows that still need a PUT: those with a local file selected
    // that haven't been uploaded (or whose previous upload failed).
    std::vector<int> pending;
    for (int i = 0; i < static_cast<int>(package_rows_.size()); ++i) {
        const auto& r = package_rows_[i];
        if (!r.local_file.isEmpty() &&
            r.state != PackageRow::State::Uploaded) {
            pending.push_back(i);
        }
    }

    if (pending.empty()) { done(true, {}); return; }

    if (!httpBaseUrl_.isValid() || httpBaseUrl_.isEmpty()) {
        done(false, tr("HTTP base URL is not configured; cannot upload."));
        return;
    }

    // Shared counter / error state across parallel PUTs. Completes the outer
    // callback when every pending request has finished, successfully or not.
    struct Ctx {
        int remaining;
        bool ok = true;
        QString err;
    };
    auto ctx = std::make_shared<Ctx>();
    ctx->remaining = static_cast<int>(pending.size());

    const auto av_id = boost::uuids::to_string(app_version_.id);
    QPointer<AppVersionDetailDialog> self = this;

    for (int row : pending) {
        auto& pr = package_rows_[row];
        setPackageRowState(row, PackageRow::State::Uploading);

        const std::string uri_path =
            ores::compute::net::compute_storage::package_path(
                av_id, pr.platform_code, ".tar.gz");
        QUrl uploadUrl = httpBaseUrl_;
        uploadUrl.setPath(QString::fromStdString(uri_path));

        auto* file = new QFile(pr.local_file, this);
        if (!file->open(QIODevice::ReadOnly)) {
            const auto msg = tr("Cannot open %1").arg(pr.local_file);
            setPackageRowState(row, PackageRow::State::Failed, msg);
            file->deleteLater();
            ctx->ok = false;
            if (ctx->err.isEmpty()) ctx->err = msg;
            if (--ctx->remaining == 0) done(ctx->ok, ctx->err);
            continue;
        }

        QNetworkRequest req{uploadUrl};
        req.setHeader(QNetworkRequest::ContentTypeHeader,
                      QByteArray("application/octet-stream"));
        auto* reply = networkManager_->put(req, file);

        // Capture platform_id, not row index: syncPackagesTab may reorder
        // package_rows_ between the PUT kicking off and finished() firing,
        // so resolve the row at callback time by id. Platform buttons are
        // disabled while saveInProgress_, so under normal use the row is
        // still present — but the defensive lookup makes it correct even
        // if a concurrent mutation slipped through.
        const std::string platform_id = pr.platform_id;
        const QString stored_uri = QString::fromStdString(uri_path);
        connect(reply, &QNetworkReply::finished, this,
                [self, reply, file, ctx, done, platform_id, stored_uri]() {
            reply->deleteLater();
            file->deleteLater();
            if (!self) return;

            auto& rows = self->package_rows_;
            const auto it = std::find_if(rows.begin(), rows.end(),
                [&](const auto& r) { return r.platform_id == platform_id; });
            const int resolved_row = (it == rows.end())
                ? -1 : static_cast<int>(it - rows.begin());

            if (reply->error() != QNetworkReply::NoError) {
                const auto detail = reply->errorString();
                if (resolved_row >= 0) {
                    self->setPackageRowState(resolved_row,
                        PackageRow::State::Failed, detail);
                }
                ctx->ok = false;
                if (ctx->err.isEmpty())
                    ctx->err = tr("%1: %2").arg(
                        resolved_row >= 0
                            ? QString::fromStdString(rows[resolved_row].platform_code)
                            : tr("(removed)"),
                        detail);
            } else if (resolved_row >= 0) {
                rows[resolved_row].remote_uri = stored_uri;
                self->setPackageRowState(resolved_row,
                    PackageRow::State::Uploaded);
            }

            // Progress is coarse: increments by completed row rather than by
            // bytes so we don't have to plumb uploadProgress signals per file.
            const int total = static_cast<int>(rows.size());
            int waiting = 0;
            for (const auto& p : rows)
                if (p.state == PackageRow::State::Uploading) ++waiting;
            const int completed = total - waiting;
            if (total > 0)
                self->ui_->packagesProgressBar->setValue(
                    completed * 100 / total);

            if (--ctx->remaining == 0) done(ctx->ok, ctx->err);
        });
    }
}

void AppVersionDetailDialog::submitSave() {
    using FutureResult = std::pair<bool, std::string>;
    QPointer<AppVersionDetailDialog> self = this;
    const compute::domain::app_version versionToSave = app_version_;

    // Translate package_rows_ into junction entries for the RPC payload.
    std::vector<compute::domain::app_version_platform> platformsToSave;
    platformsToSave.reserve(package_rows_.size());
    for (const auto& r : package_rows_) {
        compute::domain::app_version_platform avp;
        avp.tenant_id = app_version_.tenant_id;
        avp.app_version_id = app_version_.id;
        try {
            avp.platform_id = boost::lexical_cast<boost::uuids::uuid>(
                r.platform_id);
        } catch (...) { continue; }
        avp.platform_code = r.platform_code;
        avp.package_uri = r.remote_uri.toStdString();
        platformsToSave.push_back(std::move(avp));
    }

    QFuture<FutureResult> future =
        QtConcurrent::run([self, versionToSave, platformsToSave]() -> FutureResult {
            if (!self) return {false, ""};

            compute::messaging::save_app_version_request request;
            request.app_version = versionToSave;
            request.platforms = platformsToSave;

            auto result =
                self->clientManager_->process_authenticated_request(
                    std::move(request));

            if (!result) return {false, "Failed to communicate with server"};
            return {result->success, result->message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(this);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, versionToSave]() {
        if (!self) return;
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->app_versionSaved(
                QString::fromStdString(versionToSave.wrapper_version));
            self->notifySaveSuccess(
                tr("App version '%1' saved").arg(
                    QString::fromStdString(versionToSave.wrapper_version)));
        } else {
            BOOST_LOG_SEV(lg(), error) << "App version save failed: " << message;
            emit self->errorMessage(
                QString("Failed to save app version: %1").arg(
                    QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(message));
        }
    });
    watcher->setFuture(future);
}

void AppVersionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete app version while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(app_version_.wrapper_version);
    auto reply = MessageBoxHelper::question(this, "Delete App Version",
        QString("Are you sure you want to delete app version '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(
        ChangeReasonDialog::OperationType::Delete, true, "common");
    if (!crSel) return;

    // Delete not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Delete operation is not yet implemented for this entity.");
}

}
