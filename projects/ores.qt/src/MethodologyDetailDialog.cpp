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
#include "ores.qt/MethodologyDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ui_MethodologyDetailDialog.h"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq/messaging/dataset_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

MethodologyDetailDialog::MethodologyDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::MethodologyDetailDialog),
      clientManager_(nullptr),
      isCreateMode_(true),
      isReadOnly_(false) {

    ui_->setupUi(this);
    setupConnections();
}

MethodologyDetailDialog::~MethodologyDetailDialog() {
    delete ui_;
}

QTabWidget* MethodologyDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* MethodologyDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* MethodologyDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void MethodologyDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked,
            this, &MethodologyDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked,
            this, &MethodologyDetailDialog::onDeleteClicked);
}

void MethodologyDetailDialog::setCreateMode(bool create) {
    isCreateMode_ = create;
    setProvenanceEnabled(!create);
    updateUiState();
}

void MethodologyDetailDialog::setMethodology(
    const dq::domain::methodology& methodology) {
    methodology_ = methodology;

    ui_->nameEdit->setText(QString::fromStdString(methodology.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(methodology.description));

    if (methodology.logic_reference) {
        ui_->logicReferenceEdit->setText(QString::fromStdString(*methodology.logic_reference));
    }

    if (methodology.implementation_details) {
        ui_->implementationEdit->setPlainText(QString::fromStdString(*methodology.implementation_details));
    }

    populateProvenance(methodology_.version, methodology_.modified_by,
                       methodology_.performed_by, methodology_.recorded_at,
                       "", methodology_.change_commentary);

    updateUiState();
}

void MethodologyDetailDialog::setReadOnly(bool readOnly) {
    isReadOnly_ = readOnly;
    updateUiState();
}

void MethodologyDetailDialog::updateUiState() {
    ui_->nameEdit->setReadOnly(isReadOnly_);
    ui_->descriptionEdit->setReadOnly(isReadOnly_);
    ui_->logicReferenceEdit->setReadOnly(isReadOnly_);
    ui_->implementationEdit->setReadOnly(isReadOnly_);

    ui_->saveButton->setVisible(!isReadOnly_);
    ui_->deleteButton->setVisible(!isCreateMode_ && !isReadOnly_);
}

void MethodologyDetailDialog::onSaveClicked() {
    QString name = ui_->nameEdit->text().trimmed();
    QString description = ui_->descriptionEdit->toPlainText().trimmed();
    QString logicRef = ui_->logicReferenceEdit->text().trimmed();
    QString implementation = ui_->implementationEdit->toPlainText().trimmed();

    if (name.isEmpty()) {
        MessageBoxHelper::warning(this, tr("Validation Error"),
                                  tr("Name is required."));
        return;
    }

    dq::domain::methodology methodology;
    methodology.id = isCreateMode_ ? boost::uuids::random_generator()() : methodology_.id;
    methodology.name = name.toStdString();
    methodology.description = description.toStdString();
    methodology.modified_by = username_;
    methodology.version = isCreateMode_ ? 0 : methodology_.version;

    if (!logicRef.isEmpty()) {
        methodology.logic_reference = logicRef.toStdString();
    }

    if (!implementation.isEmpty()) {
        methodology.implementation_details = implementation.toStdString();
    }

    QPointer<MethodologyDetailDialog> self = this;
    boost::uuids::uuid methodologyId = methodology.id;

    struct SaveResult { bool success; std::string message; };

    auto task = [self, methodology]() -> SaveResult {
        if (!self || !self->clientManager_) return {false, "Dialog closed"};

        dq::messaging::save_methodology_request request;
        request.methodology = methodology;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_methodology_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = dq::messaging::save_methodology_response::deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this, [self, watcher, methodologyId]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            emit self->methodologySaved(methodologyId);
            self->notifySaveSuccess(tr("Methodology saved"));
        } else {
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });

    emit statusMessage(tr("Saving..."));
    watcher->setFuture(QtConcurrent::run(task));
}

void MethodologyDetailDialog::onDeleteClicked() {
    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"),
        tr("Delete methodology '%1'?").arg(ui_->nameEdit->text()),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<MethodologyDetailDialog> self = this;
    boost::uuids::uuid methodologyId = methodology_.id;

    auto task = [self, methodologyId]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_methodology_request request;
        request.ids = {methodologyId};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_methodology_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_methodology_response::deserialize(*payload_result);
        return response && !response->results.empty() && response->results[0].success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher, methodologyId]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (success) {
            emit self->statusMessage(tr("Methodology deleted successfully"));
            emit self->methodologyDeleted(methodologyId);
            self->requestClose();
        } else {
            emit self->errorMessage(tr("Failed to delete methodology"));
        }
    });

    emit statusMessage(tr("Deleting..."));
    watcher->setFuture(QtConcurrent::run(task));
}

}
