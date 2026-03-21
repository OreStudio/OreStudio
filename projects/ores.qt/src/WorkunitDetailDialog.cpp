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
#include "ores.qt/WorkunitDetailDialog.hpp"

#include <QFile>
#include <QMessageBox>
#include <QFileDialog>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QNetworkAccessManager>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ui_WorkunitDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.compute/messaging/workunit_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

WorkunitDetailDialog::WorkunitDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::WorkunitDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

WorkunitDetailDialog::~WorkunitDetailDialog() {
    delete ui_;
}

QTabWidget* WorkunitDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* WorkunitDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* WorkunitDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void WorkunitDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void WorkunitDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &WorkunitDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &WorkunitDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &WorkunitDetailDialog::onCloseClicked);
    connect(ui_->browseInputButton, &QPushButton::clicked, this,
            &WorkunitDetailDialog::onBrowseInputClicked);
    connect(ui_->browseConfigButton, &QPushButton::clicked, this,
            &WorkunitDetailDialog::onBrowseConfigClicked);

    connect(ui_->batchIdEdit, &QLineEdit::textChanged, this,
            &WorkunitDetailDialog::onFieldChanged);
    connect(ui_->appVersionIdEdit, &QLineEdit::textChanged, this,
            &WorkunitDetailDialog::onFieldChanged);
    connect(ui_->prioritySpinBox, QOverload<int>::of(&QSpinBox::valueChanged),
            this, &WorkunitDetailDialog::onFieldChanged);
    connect(ui_->redundancySpinBox, QOverload<int>::of(&QSpinBox::valueChanged),
            this, &WorkunitDetailDialog::onFieldChanged);
}

void WorkunitDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void WorkunitDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void WorkunitDetailDialog::setHttpBaseUrl(const std::string& url) {
    httpBaseUrl_ = url;
}

void WorkunitDetailDialog::setWorkunit(
    const compute::domain::workunit& workunit) {
    workunit_ = workunit;
    updateUiFromWorkunit();
}

void WorkunitDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        workunit_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void WorkunitDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->batchIdEdit->setReadOnly(readOnly);
    ui_->appVersionIdEdit->setReadOnly(readOnly);
    ui_->prioritySpinBox->setEnabled(!readOnly);
    ui_->redundancySpinBox->setEnabled(!readOnly);
    ui_->browseInputButton->setEnabled(!readOnly);
    ui_->browseConfigButton->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void WorkunitDetailDialog::updateUiFromWorkunit() {
    ui_->batchIdEdit->setText(
        QString::fromStdString(boost::uuids::to_string(workunit_.batch_id)));
    ui_->appVersionIdEdit->setText(
        QString::fromStdString(boost::uuids::to_string(workunit_.app_version_id)));
    ui_->inputFilePathEdit->setText(
        QString::fromStdString(workunit_.input_uri));
    ui_->configFilePathEdit->setText(
        QString::fromStdString(workunit_.config_uri));
    ui_->prioritySpinBox->setValue(workunit_.priority);
    ui_->redundancySpinBox->setValue(workunit_.target_redundancy);

    populateProvenance(workunit_.version,
                       workunit_.modified_by,
                       workunit_.performed_by,
                       workunit_.recorded_at,
                       workunit_.change_reason_code,
                       workunit_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void WorkunitDetailDialog::updateWorkunitFromUi() {
    try {
        boost::uuids::string_generator gen;
        const std::string batch_id_str =
            ui_->batchIdEdit->text().trimmed().toStdString();
        if (!batch_id_str.empty())
            workunit_.batch_id = gen(batch_id_str);

        const std::string app_version_id_str =
            ui_->appVersionIdEdit->text().trimmed().toStdString();
        if (!app_version_id_str.empty())
            workunit_.app_version_id = gen(app_version_id_str);
    } catch (const std::exception&) {
        // UUID parse errors are caught during validateInput()
    }

    const std::string id_str = boost::uuids::to_string(workunit_.id);
    workunit_.input_uri = "workunits/" + id_str + "/input";
    workunit_.config_uri = "workunits/" + id_str + "/config";
    workunit_.priority = ui_->prioritySpinBox->value();
    workunit_.target_redundancy = ui_->redundancySpinBox->value();
    workunit_.modified_by = username_;
    workunit_.performed_by = username_;
}

void WorkunitDetailDialog::onBrowseInputClicked() {
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Select Input File"),
        QString(),
        tr("Zip Files (*.zip);;All Files (*)"));

    if (!path.isEmpty()) {
        selectedInputFilePath_ = path;
        ui_->inputFilePathEdit->setText(path);
        hasChanges_ = true;
        updateSaveButtonState();
    }
}

void WorkunitDetailDialog::onBrowseConfigClicked() {
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Select Config File"),
        QString(),
        tr("XML Files (*.xml);;All Files (*)"));

    if (!path.isEmpty()) {
        selectedConfigFilePath_ = path;
        ui_->configFilePathEdit->setText(path);
        hasChanges_ = true;
        updateSaveButtonState();
    }
}

void WorkunitDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void WorkunitDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool WorkunitDetailDialog::validateInput() {
    const QString batch_id_str = ui_->batchIdEdit->text().trimmed();
    const QString app_version_id_str = ui_->appVersionIdEdit->text().trimmed();

    if (batch_id_str.isEmpty() || app_version_id_str.isEmpty())
        return false;

    // Validate UUID format
    try {
        boost::uuids::string_generator gen;
        gen(batch_id_str.toStdString());
        gen(app_version_id_str.toStdString());
    } catch (const std::exception&) {
        return false;
    }

    // Files required for create mode
    if (createMode_) {
        if (selectedInputFilePath_.isEmpty() || selectedConfigFilePath_.isEmpty())
            return false;
    }

    return true;
}

void WorkunitDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot submit workunit while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in Batch ID and App Version ID (valid UUIDs), "
            "and select input and config files.");
        return;
    }

    if (httpBaseUrl_.empty()) {
        MessageBoxHelper::warning(this, "No Server URL",
            "HTTP base URL is not configured. Cannot upload files.");
        return;
    }

    updateWorkunitFromUi();

    const auto crOpType = createMode_
        ? ChangeReasonDialog::OperationType::Create
        : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_,
        createMode_ ? "system" : "common");
    if (!crSel) return;
    workunit_.change_reason_code = crSel->reason_code;
    workunit_.change_commentary = crSel->commentary;

    BOOST_LOG_SEV(lg(), info) << "Submitting workunit: "
                              << boost::uuids::to_string(workunit_.id);

    const std::string id_str = boost::uuids::to_string(workunit_.id);

    // Phase 1: upload input file
    if (!selectedInputFilePath_.isEmpty()) {
        const QString inputUrl = QString::fromStdString(
            httpBaseUrl_ + "/workunits/" + id_str + "/input");

        auto* inputFile = new QFile(selectedInputFilePath_, this);
        if (!inputFile->open(QIODevice::ReadOnly)) {
            MessageBoxHelper::critical(this, "File Error",
                tr("Cannot open input file: %1").arg(selectedInputFilePath_));
            inputFile->deleteLater();
            return;
        }

        ui_->saveButton->setEnabled(false);

        auto* nm = new QNetworkAccessManager(this);
        QNetworkRequest req{QUrl(inputUrl)};
        req.setHeader(QNetworkRequest::ContentTypeHeader,
                      QByteArray("application/octet-stream"));

        QPointer<WorkunitDetailDialog> self = this;
        auto* inputReply = nm->post(req, inputFile);

        connect(inputReply, &QNetworkReply::finished, this,
                [self, inputReply, inputFile, nm, id_str]() {
            if (!self) {
                inputReply->deleteLater();
                inputFile->deleteLater();
                nm->deleteLater();
                return;
            }

            inputReply->deleteLater();
            inputFile->deleteLater();
            nm->deleteLater();

            if (inputReply->error() != QNetworkReply::NoError) {
                BOOST_LOG_SEV(lg(), error) << "Input file upload failed: "
                    << inputReply->errorString().toStdString();
                MessageBoxHelper::critical(self, tr("Upload Failed"),
                    inputReply->errorString());
                self->ui_->saveButton->setEnabled(true);
                return;
            }

            BOOST_LOG_SEV(lg(), info) << "Input file uploaded for workunit: "
                                      << id_str;

            // Phase 2: upload config file
            if (!self->selectedConfigFilePath_.isEmpty()) {
                const QString configUrl = QString::fromStdString(
                    self->httpBaseUrl_ + "/workunits/" + id_str + "/config");

                auto* configFile = new QFile(self->selectedConfigFilePath_, self);
                if (!configFile->open(QIODevice::ReadOnly)) {
                    MessageBoxHelper::critical(self, "File Error",
                        tr("Cannot open config file: %1").arg(
                            self->selectedConfigFilePath_));
                    configFile->deleteLater();
                    self->ui_->saveButton->setEnabled(true);
                    return;
                }

                auto* nm2 = new QNetworkAccessManager(self);
                QNetworkRequest req2{QUrl(configUrl)};
                req2.setHeader(QNetworkRequest::ContentTypeHeader,
                               QByteArray("application/octet-stream"));

                auto* configReply = nm2->post(req2, configFile);

                connect(configReply, &QNetworkReply::finished, self,
                        [self, configReply, configFile, nm2, id_str]() {
                    if (!self) {
                        configReply->deleteLater();
                        configFile->deleteLater();
                        nm2->deleteLater();
                        return;
                    }

                    configReply->deleteLater();
                    configFile->deleteLater();
                    nm2->deleteLater();

                    if (configReply->error() != QNetworkReply::NoError) {
                        BOOST_LOG_SEV(lg(), error) << "Config file upload failed: "
                            << configReply->errorString().toStdString();
                        MessageBoxHelper::critical(self, tr("Upload Failed"),
                            configReply->errorString());
                        self->ui_->saveButton->setEnabled(true);
                        return;
                    }

                    BOOST_LOG_SEV(lg(), info) << "Config file uploaded for workunit: "
                                              << id_str;

                    // Phase 3: save workunit via NATS
                    self->saveWorkunitViaNats();
                });
            } else {
                // No config file to upload, proceed to NATS save
                self->saveWorkunitViaNats();
            }
        });
    } else {
        // No files to upload, submit directly via NATS
        saveWorkunitViaNats();
    }
}

void WorkunitDetailDialog::saveWorkunitViaNats() {
    using FutureResult = std::pair<bool, std::string>;
    QPointer<WorkunitDetailDialog> self = this;
    const compute::domain::workunit workunitToSave = workunit_;

    QFuture<FutureResult> future =
        QtConcurrent::run([self, workunitToSave]() -> FutureResult {
            if (!self) return {false, ""};

            compute::messaging::save_workunit_request request;
            request.workunit = workunitToSave;

            auto result =
                self->clientManager_->process_authenticated_request(
                    std::move(request));

            if (!result) return {false, "Failed to communicate with server"};
            return {result->success, result->message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(this);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, workunitToSave]() {
        if (!self) return;
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            const QString idStr = QString::fromStdString(
                boost::uuids::to_string(workunitToSave.id));
            emit self->workunitSaved(idStr);
            self->notifySaveSuccess(
                tr("Workunit submitted successfully (ID: %1)").arg(idStr));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Workunit save failed: " << message;
            emit self->errorMessage(
                QString("Failed to submit workunit: %1").arg(
                    QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Submit Failed",
                QString::fromStdString(message));
            self->ui_->saveButton->setEnabled(true);
        }
    });
    watcher->setFuture(future);
}

void WorkunitDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete workunit while disconnected from server.");
        return;
    }

    const QString id = QString::fromStdString(
        boost::uuids::to_string(workunit_.id));
    auto reply = MessageBoxHelper::question(this, "Delete Workunit",
        QString("Are you sure you want to delete workunit '%1'?").arg(id),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes)
        return;

    {
        const auto crSel = promptChangeReason(
            ChangeReasonDialog::OperationType::Delete, true, "common");
        if (!crSel) return;
    }

    // Delete not yet implemented for compute entities
    MessageBoxHelper::warning(this, "Not Implemented",
        "Delete operation is not yet implemented for this entity.");
}

}
