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
#include <QMessageBox>
#include <QFileDialog>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QNetworkAccessManager>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ui_AppVersionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.compute/messaging/app_version_protocol.hpp"

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

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &AppVersionDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &AppVersionDetailDialog::onFieldChanged);
    connect(ui_->platformEdit, &QLineEdit::textChanged, this,
            &AppVersionDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &AppVersionDetailDialog::onFieldChanged);
}

void AppVersionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void AppVersionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void AppVersionDetailDialog::setHttpBaseUrl(const std::string& url) {
    httpBaseUrl_ = url;
}

void AppVersionDetailDialog::setVersion(
    const compute::domain::app_version& app_version) {
    app_version_ = app_version;
    updateUiFromVersion();
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
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->platformEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->minRamSpinBox->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
    ui_->browsePackageButton->setEnabled(!readOnly);
}

void AppVersionDetailDialog::updateUiFromVersion() {
    ui_->codeEdit->setText(QString::fromStdString(app_version_.wrapper_version));
    ui_->nameEdit->setText(QString::fromStdString(app_version_.engine_version));
    ui_->platformEdit->setText(QString::fromStdString(app_version_.platform));
    ui_->minRamSpinBox->setValue(app_version_.min_ram_mb);
    ui_->descriptionEdit->setPlainText(QString::fromStdString(app_version_.package_uri));

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
    if (createMode_) {
        app_version_.wrapper_version = ui_->codeEdit->text().trimmed().toStdString();
    }
    app_version_.engine_version = ui_->nameEdit->text().trimmed().toStdString();
    app_version_.platform = ui_->platformEdit->text().trimmed().toStdString();
    app_version_.min_ram_mb = ui_->minRamSpinBox->value();
    app_version_.package_uri = "packages/" +
        boost::uuids::to_string(app_version_.id) + "/package";
    app_version_.modified_by = username_;
    app_version_.performed_by = username_;
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
    return !wrapper_version.isEmpty() && !engine_version.isEmpty();
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

    if (httpBaseUrl_.empty()) {
        MessageBoxHelper::warning(this, "No Server URL",
            "HTTP base URL is not configured. Cannot upload package.");
        return;
    }

    const std::string id_str = boost::uuids::to_string(app_version_.id);
    const QString url = QString::fromStdString(
        httpBaseUrl_ + "/packages/" + id_str + "/package");

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
    QNetworkRequest request{QUrl(url)};
    request.setHeader(QNetworkRequest::ContentTypeHeader,
                      QByteArray("application/octet-stream"));

    QPointer<AppVersionDetailDialog> self = this;
    auto* reply = networkManager->post(request, file);

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
            BOOST_LOG_SEV(lg(), error) << "Package upload failed: "
                                       << reply->errorString().toStdString();
            MessageBoxHelper::critical(self, tr("Upload Failed"),
                reply->errorString());
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

    QFuture<FutureResult> future =
        QtConcurrent::run([self, versionToSave]() -> FutureResult {
            if (!self) return {false, ""};

            compute::messaging::save_app_version_request request;
            request.app_version = versionToSave;

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
