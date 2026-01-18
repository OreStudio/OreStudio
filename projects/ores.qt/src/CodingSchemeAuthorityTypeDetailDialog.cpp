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
#include "ores.qt/CodingSchemeAuthorityTypeDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_CodingSchemeAuthorityTypeDetailDialog.h"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

CodingSchemeAuthorityTypeDetailDialog::CodingSchemeAuthorityTypeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::CodingSchemeAuthorityTypeDetailDialog),
      clientManager_(nullptr),
      isCreateMode_(true),
      isReadOnly_(false) {

    ui_->setupUi(this);
    setupConnections();
}

CodingSchemeAuthorityTypeDetailDialog::~CodingSchemeAuthorityTypeDetailDialog() {
    delete ui_;
}

void CodingSchemeAuthorityTypeDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked,
            this, &CodingSchemeAuthorityTypeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked,
            this, &CodingSchemeAuthorityTypeDetailDialog::onDeleteClicked);
}

void CodingSchemeAuthorityTypeDetailDialog::setCreateMode(bool create) {
    isCreateMode_ = create;
    updateUiState();
}

void CodingSchemeAuthorityTypeDetailDialog::setAuthorityType(
    const dq::domain::coding_scheme_authority_type& authorityType) {
    authorityType_ = authorityType;

    ui_->codeEdit->setText(QString::fromStdString(authorityType.code));
    ui_->nameEdit->setText(QString::fromStdString(authorityType.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(authorityType.description));
    ui_->commentaryEdit->setPlainText(QString::fromStdString(authorityType.change_commentary));

    updateUiState();
}

void CodingSchemeAuthorityTypeDetailDialog::setReadOnly(bool readOnly) {
    isReadOnly_ = readOnly;
    updateUiState();
}

void CodingSchemeAuthorityTypeDetailDialog::updateUiState() {
    ui_->codeEdit->setReadOnly(!isCreateMode_ || isReadOnly_);
    ui_->nameEdit->setReadOnly(isReadOnly_);
    ui_->descriptionEdit->setReadOnly(isReadOnly_);
    ui_->commentaryEdit->setReadOnly(isReadOnly_);

    ui_->saveButton->setVisible(!isReadOnly_);
    ui_->deleteButton->setVisible(!isCreateMode_ && !isReadOnly_);
}

void CodingSchemeAuthorityTypeDetailDialog::onSaveClicked() {
    QString code = ui_->codeEdit->text().trimmed();
    QString name = ui_->nameEdit->text().trimmed();
    QString description = ui_->descriptionEdit->toPlainText().trimmed();
    QString commentary = ui_->commentaryEdit->toPlainText().trimmed();

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

    dq::domain::coding_scheme_authority_type at;
    at.code = code.toStdString();
    at.name = name.toStdString();
    at.description = description.toStdString();
    at.change_commentary = commentary.toStdString();
    at.recorded_by = username_;
    at.version = isCreateMode_ ? 0 : authorityType_.version;

    QPointer<CodingSchemeAuthorityTypeDetailDialog> self = this;

    struct SaveResult { bool success; std::string message; };

    auto task = [self, at]() -> SaveResult {
        if (!self || !self->clientManager_) return {false, "Dialog closed"};

        dq::messaging::save_coding_scheme_authority_type_request request;
        request.authority_type = at;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_coding_scheme_authority_type_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = dq::messaging::save_coding_scheme_authority_type_response::deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this, [self, watcher, code]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            emit self->statusMessage(tr("Authority type saved successfully"));
            emit self->authorityTypeSaved(code);
            self->requestClose();
        } else {
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });

    emit statusMessage(tr("Saving..."));
    watcher->setFuture(QtConcurrent::run(task));
}

void CodingSchemeAuthorityTypeDetailDialog::onDeleteClicked() {
    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"),
        tr("Delete authority type '%1'?").arg(ui_->codeEdit->text()),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<CodingSchemeAuthorityTypeDetailDialog> self = this;
    QString code = ui_->codeEdit->text();

    auto task = [self, code]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_coding_scheme_authority_type_request request;
        request.codes = {code.toStdString()};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_coding_scheme_authority_type_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_coding_scheme_authority_type_response::deserialize(*payload_result);
        return response && !response->results.empty() && response->results[0].success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher, code]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (success) {
            emit self->statusMessage(tr("Authority type deleted successfully"));
            emit self->authorityTypeDeleted(code);
            self->requestClose();
        } else {
            emit self->errorMessage(tr("Failed to delete authority type"));
        }
    });

    emit statusMessage(tr("Deleting..."));
    watcher->setFuture(QtConcurrent::run(task));
}

}
