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
#include <QFileInfo>
#include <QMessageBox>
#include <QFileDialog>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QNetworkAccessManager>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ui_WorkunitDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.compute.api/messaging/workunit_protocol.hpp"

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

    connect(ui_->batchCombo, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &WorkunitDetailDialog::onFieldChanged);
    connect(ui_->appVersionCombo, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &WorkunitDetailDialog::onFieldChanged);
    connect(ui_->prioritySpinBox, QOverload<int>::of(&QSpinBox::valueChanged),
            this, &WorkunitDetailDialog::onFieldChanged);
    connect(ui_->redundancySpinBox, QOverload<int>::of(&QSpinBox::valueChanged),
            this, &WorkunitDetailDialog::onFieldChanged);
}

void WorkunitDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    loadBatches();
    loadAppVersions();
}

void WorkunitDetailDialog::loadBatches() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<WorkunitDetailDialog> self = this;

    struct FetchResult {
        bool success;
        std::vector<IdEntry> entries;
        std::string error;
    };

    auto task = [self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {false, {}, "Dialog closed"};

        compute::messaging::list_batches_request request;
        request.limit = 1000;
        auto result = self->clientManager_->process_authenticated_request(
            std::move(request));

        if (!result)
            return {false, {}, "Failed to fetch batches"};

        std::vector<IdEntry> entries;
        entries.reserve(result->batches.size());
        for (const auto& batch : result->batches) {
            entries.push_back({
                boost::uuids::to_string(batch.id),
                batch.external_ref
            });
        }
        return {true, std::move(entries), {}};
    };

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    connect(watcher, &QFutureWatcher<FetchResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (self && result.success) {
            self->batchEntries_ = std::move(result.entries);
            self->populateBatchCombo();
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void WorkunitDetailDialog::loadAppVersions() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<WorkunitDetailDialog> self = this;

    struct FetchResult {
        bool success;
        std::vector<IdEntry> entries;
        std::string error;
    };

    auto task = [self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {false, {}, "Dialog closed"};

        compute::messaging::list_app_versions_request request;
        request.limit = 1000;
        auto result = self->clientManager_->process_authenticated_request(
            std::move(request));

        if (!result)
            return {false, {}, "Failed to fetch app versions"};

        std::vector<IdEntry> entries;
        entries.reserve(result->app_versions.size());
        for (const auto& av : result->app_versions) {
            entries.push_back({
                boost::uuids::to_string(av.id),
                av.wrapper_version + " / " + av.engine_version
            });
        }
        return {true, std::move(entries), {}};
    };

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    connect(watcher, &QFutureWatcher<FetchResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (self && result.success) {
            self->appVersionEntries_ = std::move(result.entries);
            self->populateAppVersionCombo();
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void WorkunitDetailDialog::populateBatchCombo() {
    ui_->batchCombo->blockSignals(true);
    ui_->batchCombo->clear();
    ui_->batchCombo->addItem(tr("(select batch)"), QString());
    for (const auto& entry : batchEntries_) {
        ui_->batchCombo->addItem(
            QString::fromStdString(entry.label),
            QString::fromStdString(entry.id));
    }
    if (!workunit_.batch_id.is_nil()) {
        const QString idStr = QString::fromStdString(
            boost::uuids::to_string(workunit_.batch_id));
        for (int i = 0; i < ui_->batchCombo->count(); ++i) {
            if (ui_->batchCombo->itemData(i).toString() == idStr) {
                ui_->batchCombo->setCurrentIndex(i);
                break;
            }
        }
    }
    ui_->batchCombo->blockSignals(false);
}

void WorkunitDetailDialog::populateAppVersionCombo() {
    ui_->appVersionCombo->blockSignals(true);
    ui_->appVersionCombo->clear();
    ui_->appVersionCombo->addItem(tr("(select app version)"), QString());
    for (const auto& entry : appVersionEntries_) {
        ui_->appVersionCombo->addItem(
            QString::fromStdString(entry.label),
            QString::fromStdString(entry.id));
    }
    if (!workunit_.app_version_id.is_nil()) {
        const QString idStr = QString::fromStdString(
            boost::uuids::to_string(workunit_.app_version_id));
        for (int i = 0; i < ui_->appVersionCombo->count(); ++i) {
            if (ui_->appVersionCombo->itemData(i).toString() == idStr) {
                ui_->appVersionCombo->setCurrentIndex(i);
                break;
            }
        }
    }
    ui_->appVersionCombo->blockSignals(false);
}

void WorkunitDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void WorkunitDetailDialog::setHttpBaseUrl(const std::string& url) {
    httpBaseUrl_ = QUrl(QString::fromStdString(url));
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
    ui_->batchCombo->setEnabled(!readOnly);
    ui_->appVersionCombo->setEnabled(!readOnly);
    ui_->prioritySpinBox->setEnabled(!readOnly);
    ui_->redundancySpinBox->setEnabled(!readOnly);
    ui_->browseInputButton->setEnabled(!readOnly);
    ui_->browseConfigButton->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void WorkunitDetailDialog::updateUiFromWorkunit() {
    // Select batch in combo (if already loaded)
    if (!workunit_.batch_id.is_nil()) {
        const QString idStr = QString::fromStdString(
            boost::uuids::to_string(workunit_.batch_id));
        for (int i = 0; i < ui_->batchCombo->count(); ++i) {
            if (ui_->batchCombo->itemData(i).toString() == idStr) {
                ui_->batchCombo->setCurrentIndex(i);
                break;
            }
        }
    }

    // Select app version in combo (if already loaded)
    if (!workunit_.app_version_id.is_nil()) {
        const QString idStr = QString::fromStdString(
            boost::uuids::to_string(workunit_.app_version_id));
        for (int i = 0; i < ui_->appVersionCombo->count(); ++i) {
            if (ui_->appVersionCombo->itemData(i).toString() == idStr) {
                ui_->appVersionCombo->setCurrentIndex(i);
                break;
            }
        }
    }

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
    const QString batchIdStr = ui_->batchCombo->currentData().toString();
    if (!batchIdStr.isEmpty()) {
        try {
            workunit_.batch_id = boost::lexical_cast<boost::uuids::uuid>(
                batchIdStr.toStdString());
        } catch (...) {}
    }

    const QString appVersionIdStr = ui_->appVersionCombo->currentData().toString();
    if (!appVersionIdStr.isEmpty()) {
        try {
            workunit_.app_version_id = boost::lexical_cast<boost::uuids::uuid>(
                appVersionIdStr.toStdString());
        } catch (...) {}
    }

    const std::string id_str = boost::uuids::to_string(workunit_.id);
    {
        // Preserve the full extension (e.g. ".csv") for local inspection.
        const std::string base = "api/v1/compute/workunits/" + id_str + "/input";
        if (!selectedInputFilePath_.isEmpty()) {
            const std::string ext =
                QFileInfo(selectedInputFilePath_).completeSuffix().toStdString();
            workunit_.input_uri = base + (ext.empty() ? "" : "." + ext);
        } else if (workunit_.input_uri.empty()) {
            workunit_.input_uri = base;
        }
    }
    if (!selectedConfigFilePath_.isEmpty()) {
        const std::string base = "api/v1/compute/workunits/" + id_str + "/config";
        const std::string ext =
            QFileInfo(selectedConfigFilePath_).completeSuffix().toStdString();
        workunit_.config_uri = base + (ext.empty() ? "" : "." + ext);
    } else if (workunit_.config_uri.empty()) {
        workunit_.config_uri = std::string{};
    }
    workunit_.priority = ui_->prioritySpinBox->value();
    workunit_.target_redundancy = ui_->redundancySpinBox->value();
    workunit_.modified_by = username_;
    workunit_.performed_by = username_;
}

void WorkunitDetailDialog::onBrowseInputClicked() {
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Select Input File"),
        QString(),
        tr("Archive Files (*.zip *.tar.gz *.tgz);;All Files (*)"));

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
    const QString batch_id = ui_->batchCombo->currentData().toString();
    const QString app_version_id = ui_->appVersionCombo->currentData().toString();

    if (batch_id.isEmpty() || app_version_id.isEmpty())
        return false;

    // Input file required for create mode; config is optional
    if (createMode_) {
        if (selectedInputFilePath_.isEmpty())
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
            "Please select a Batch, an App Version, and an input file.");
        return;
    }

    if (!httpBaseUrl_.isValid() || httpBaseUrl_.isEmpty()) {
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
        const QString ext_in = QFileInfo(selectedInputFilePath_).completeSuffix();
        QUrl inputUrl = httpBaseUrl_;
        inputUrl.setPath("/api/v1/compute/workunits/"
            + QString::fromStdString(id_str) + "/input"
            + (ext_in.isEmpty() ? QString{} : "." + ext_in));

        auto* inputFile = new QFile(selectedInputFilePath_, this);
        if (!inputFile->open(QIODevice::ReadOnly)) {
            MessageBoxHelper::critical(this, "File Error",
                tr("Cannot open input file: %1").arg(selectedInputFilePath_));
            inputFile->deleteLater();
            return;
        }

        ui_->saveButton->setEnabled(false);

        auto* nm = new QNetworkAccessManager(this);
        QNetworkRequest req{inputUrl};
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
                const auto body = inputReply->readAll();
                const auto detail = body.isEmpty()
                    ? inputReply->errorString()
                    : inputReply->errorString() + "\n" + QString::fromUtf8(body);
                BOOST_LOG_SEV(lg(), error) << "Input file upload failed: "
                    << detail.toStdString();
                MessageBoxHelper::critical(self, tr("Upload Failed"), detail);
                self->ui_->saveButton->setEnabled(true);
                return;
            }

            BOOST_LOG_SEV(lg(), info) << "Input file uploaded for workunit: "
                                      << id_str;

            // Phase 2: upload config file
            if (!self->selectedConfigFilePath_.isEmpty()) {
                const QString ext_cfg =
                    QFileInfo(self->selectedConfigFilePath_).completeSuffix();
                QUrl configUrl = self->httpBaseUrl_;
                configUrl.setPath("/api/v1/compute/workunits/"
                    + QString::fromStdString(id_str) + "/config"
                    + (ext_cfg.isEmpty() ? QString{} : "." + ext_cfg));

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
                QNetworkRequest req2{configUrl};
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
                        const auto body = configReply->readAll();
                        const auto detail = body.isEmpty()
                            ? configReply->errorString()
                            : configReply->errorString() + "\n" + QString::fromUtf8(body);
                        BOOST_LOG_SEV(lg(), error) << "Config file upload failed: "
                            << detail.toStdString();
                        MessageBoxHelper::critical(self, tr("Upload Failed"), detail);
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
