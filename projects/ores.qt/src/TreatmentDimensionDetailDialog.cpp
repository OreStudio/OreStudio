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
#include "ores.qt/TreatmentDimensionDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_TreatmentDimensionDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

TreatmentDimensionDetailDialog::TreatmentDimensionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::TreatmentDimensionDetailDialog),
      clientManager_(nullptr),
      isCreateMode_(true),
      isReadOnly_(false) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupConnections();
}

TreatmentDimensionDetailDialog::~TreatmentDimensionDetailDialog() {
    delete ui_;
}

QTabWidget* TreatmentDimensionDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* TreatmentDimensionDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* TreatmentDimensionDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void TreatmentDimensionDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked,
            this, &TreatmentDimensionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked,
            this, &TreatmentDimensionDetailDialog::onDeleteClicked);
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &TreatmentDimensionDetailDialog::onCloseClicked);
}

void TreatmentDimensionDetailDialog::setCreateMode(bool create) {
    isCreateMode_ = create;
    updateUiState();
}

void TreatmentDimensionDetailDialog::setDimension(
    const dq::domain::treatment_dimension& dimension) {
    dimension_ = dimension;

    ui_->codeEdit->setText(QString::fromStdString(dimension.code));
    ui_->nameEdit->setText(QString::fromStdString(dimension.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(dimension.description));

    populateProvenance(dimension_.version, dimension_.modified_by,
        dimension_.performed_by, dimension_.recorded_at,
        "", dimension_.change_commentary);

    updateUiState();
}

void TreatmentDimensionDetailDialog::setReadOnly(bool readOnly) {
    isReadOnly_ = readOnly;
    updateUiState();
}

void TreatmentDimensionDetailDialog::updateUiState() {
    ui_->codeEdit->setReadOnly(!isCreateMode_ || isReadOnly_);
    ui_->nameEdit->setReadOnly(isReadOnly_);
    ui_->descriptionEdit->setReadOnly(isReadOnly_);

    ui_->saveButton->setVisible(!isReadOnly_);
    ui_->deleteButton->setVisible(!isCreateMode_ && !isReadOnly_);
    setProvenanceEnabled(!isCreateMode_);
}

void TreatmentDimensionDetailDialog::onSaveClicked() {
    QString code = ui_->codeEdit->text().trimmed();
    QString name = ui_->nameEdit->text().trimmed();
    QString description = ui_->descriptionEdit->toPlainText().trimmed();

    if (code.isEmpty()) {
        MessageBoxHelper::warning(this, tr("Validation Error"),
                                  tr("Code is required."));
        return;
    }

    if (name.isEmpty()) {
        MessageBoxHelper::warning(this, tr("Validation Error"),
                                  tr("Name is required."));
        return;
    }

    dq::domain::treatment_dimension dim;
    dim.code = code.toStdString();
    dim.name = name.toStdString();
    dim.description = description.toStdString();
    dim.modified_by = username_;
    dim.version = isCreateMode_ ? 0 : dimension_.version;

    QPointer<TreatmentDimensionDetailDialog> self = this;

    struct SaveResult { bool success; std::string message; };

    auto task = [self, dim]() -> SaveResult {
        if (!self || !self->clientManager_) return {false, "Dialog closed"};

        dq::messaging::save_treatment_dimension_request request;
        request.dimension = dim;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_treatment_dimension_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = dq::messaging::save_treatment_dimension_response::deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this, [self, watcher, code]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            emit self->dimensionSaved(code);
            self->notifySaveSuccess(tr("Treatment dimension '%1' saved").arg(code));
        } else {
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });

    emit statusMessage(tr("Saving..."));
    watcher->setFuture(QtConcurrent::run(task));
}

void TreatmentDimensionDetailDialog::onDeleteClicked() {
    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"),
        tr("Delete treatment dimension '%1'?").arg(ui_->codeEdit->text()),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<TreatmentDimensionDetailDialog> self = this;
    QString code = ui_->codeEdit->text();

    auto task = [self, code]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_treatment_dimension_request request;
        request.codes = {code.toStdString()};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_treatment_dimension_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_treatment_dimension_response::deserialize(*payload_result);
        return response && !response->results.empty() && response->results[0].success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher, code]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (success) {
            emit self->statusMessage(tr("Treatment dimension deleted successfully"));
            emit self->dimensionDeleted(code);
            self->requestClose();
        } else {
            emit self->errorMessage(tr("Failed to delete treatment dimension"));
        }
    });

    emit statusMessage(tr("Deleting..."));
    watcher->setFuture(QtConcurrent::run(task));
}

}
