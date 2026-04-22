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
#include <QMessageBox>
#include <QFileDialog>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <QListWidgetItem>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QNetworkAccessManager>
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
      clientManager_(nullptr) {

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

    ui_->uploadPackageButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor));
    ui_->uploadPackageButton->setToolTip(
        tr("Upload the selected package file to the server"));
}

void AppVersionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onCloseClicked);
    connect(ui_->browsePackageButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onBrowsePackageClicked);
    connect(ui_->uploadPackageButton, &QPushButton::clicked, this,
            &AppVersionDetailDialog::onUploadPackageClicked);

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
        const bool assigned = std::find_if(platform_rows_.begin(),
            platform_rows_.end(), [&](const auto& row) {
                return boost::uuids::to_string(row.platform_id) == p.id;
            }) != platform_rows_.end();

        auto* item = new QListWidgetItem(QString::fromStdString(p.display_name));
        item->setData(Qt::UserRole, QString::fromStdString(p.id));

        if (assigned)
            ui_->assignedPlatformsList->addItem(item);
        else
            ui_->availablePlatformsList->addItem(item);
    }
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
    // junction fetch returns and fills platform_rows_. In create mode there
    // is nothing to load because the id has just been generated.
    platform_rows_.clear();
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
        self->platform_rows_ = std::move(result.rows);
        self->populatePlatformsTab();
        // Re-populating the assigned list flags the dialog as dirty via the
        // selection-changed signal; reset since the fetch is not a user edit.
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
    ui_->browsePackageButton->setEnabled(!readOnly);
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

    // Rebuild platform rows from the assigned list. Delegate URI layout to
    // compute_storage::package_path so the canonical per-triplet key lives
    // with the rest of the bucket convention, not inline in the UI.
    platform_rows_.clear();
    const auto av_id_str = boost::uuids::to_string(app_version_.id);
    for (int i = 0; i < ui_->assignedPlatformsList->count(); ++i) {
        const auto* item = ui_->assignedPlatformsList->item(i);
        if (!item) continue;
        const auto id_str = item->data(Qt::UserRole).toString().toStdString();
        const auto avail = std::find_if(availablePlatforms_.begin(),
            availablePlatforms_.end(),
            [&](const auto& p) { return p.id == id_str; });
        if (avail == availablePlatforms_.end()) continue;

        compute::domain::app_version_platform row;
        row.tenant_id = app_version_.tenant_id;
        row.app_version_id = app_version_.id;
        try {
            row.platform_id =
                boost::lexical_cast<boost::uuids::uuid>(avail->id);
        } catch (...) { continue; }
        row.platform_code = avail->code;
        row.package_uri = ores::compute::net::compute_storage::package_path(
            av_id_str, avail->code, ".tar.gz");
        platform_rows_.push_back(std::move(row));
    }
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

    // Require at least one platform to be assigned
    return ui_->assignedPlatformsList->count() > 0;
}

void AppVersionDetailDialog::onBrowsePackageClicked() {
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Select Engine Package"),
        QString(),
        tr("Package Files (*.tar.gz);;All Files (*)"));

    if (!path.isEmpty()) {
        selectedPackageFilePath_ = path;
        ui_->packageFilePathEdit->setText(path);
        ui_->uploadPackageButton->setEnabled(true);
    }
}

void AppVersionDetailDialog::onUploadPackageClicked() {
    if (selectedPackageFilePath_.isEmpty()) {
        MessageBoxHelper::warning(this, "No File Selected",
            "Please browse for a package file first.");
        return;
    }

    if (!httpBaseUrl_.isValid() || httpBaseUrl_.isEmpty()) {
        MessageBoxHelper::warning(this, "No Server URL",
            "HTTP base URL is not configured. Cannot upload package.");
        return;
    }

    const std::string id_str = boost::uuids::to_string(app_version_.id);
    const QString path = "/api/v1/storage/compute/packages/"
        + QString::fromStdString(id_str) ;
    QUrl uploadUrl = httpBaseUrl_;
    uploadUrl.setPath(path);

    BOOST_LOG_SEV(lg(), info) << "Uploading package to: " << uploadUrl.toString().toStdString()
                              << " file: " << selectedPackageFilePath_.toStdString();

    auto* file = new QFile(selectedPackageFilePath_, this);
    if (!file->open(QIODevice::ReadOnly)) {
        MessageBoxHelper::critical(this, "File Error",
            tr("Cannot open file: %1").arg(selectedPackageFilePath_));
        file->deleteLater();
        return;
    }

    ui_->uploadPackageButton->setEnabled(false);
    ui_->uploadPackageButton->setText(tr("Uploading..."));

    auto* networkManager = new QNetworkAccessManager(this);
    QNetworkRequest request{uploadUrl};
    request.setHeader(QNetworkRequest::ContentTypeHeader,
                      QByteArray("application/octet-stream"));

    QPointer<AppVersionDetailDialog> self = this;
    auto* reply = networkManager->put(request, file);

    connect(reply, &QNetworkReply::finished, this,
            [self, reply, file, networkManager]() {
        if (!self) { reply->deleteLater(); file->deleteLater();
                     networkManager->deleteLater(); return; }

        reply->deleteLater();
        file->deleteLater();
        networkManager->deleteLater();

        self->ui_->uploadPackageButton->setEnabled(true);
        self->ui_->uploadPackageButton->setText(tr("Upload Package"));

        if (reply->error() != QNetworkReply::NoError) {
            const auto body = reply->readAll();
            const auto detail = body.isEmpty()
                ? reply->errorString()
                : reply->errorString() + "\n" + QString::fromUtf8(body);
            BOOST_LOG_SEV(lg(), error) << "Package upload failed: "
                                       << detail.toStdString();
            MessageBoxHelper::critical(self, tr("Upload Failed"), detail);
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Package uploaded successfully for app_version: "
                                  << boost::uuids::to_string(self->app_version_.id);

        emit self->statusMessage(tr("Package uploaded successfully"));
        MessageBoxHelper::information(self, tr("Upload Complete"),
            tr("Package uploaded successfully."));
    });
}

void AppVersionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save app version while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields (wrapper version, engine version).");
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

    using FutureResult = std::pair<bool, std::string>;
    QPointer<AppVersionDetailDialog> self = this;
    const compute::domain::app_version versionToSave = app_version_;
    const auto platformsToSave = platform_rows_;

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
