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
#include "ores.qt/CodingSchemeDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_CodingSchemeDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

CodingSchemeDetailDialog::CodingSchemeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::CodingSchemeDetailDialog),
      clientManager_(nullptr),
      isCreateMode_(true),
      isReadOnly_(false) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupConnections();
}

CodingSchemeDetailDialog::~CodingSchemeDetailDialog() {
    delete ui_;
}

QTabWidget* CodingSchemeDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* CodingSchemeDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* CodingSchemeDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void CodingSchemeDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked,
            this, &CodingSchemeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked,
            this, &CodingSchemeDetailDialog::onDeleteClicked);
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &CodingSchemeDetailDialog::onCloseClicked);
}

void CodingSchemeDetailDialog::loadLookupData() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<CodingSchemeDetailDialog> self = this;

    // Load authority types
    auto authTask = [self]() -> std::vector<std::string> {
        if (!self || !self->clientManager_) return {};

        dq::messaging::get_coding_scheme_authority_types_request request;
        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_coding_scheme_authority_types_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {};

        auto response = dq::messaging::get_coding_scheme_authority_types_response::deserialize(*payload_result);
        if (!response) return {};

        std::vector<std::string> codes;
        for (const auto& at : response->authority_types) {
            codes.push_back(at.code);
        }
        return codes;
    };

    auto* authWatcher = new QFutureWatcher<std::vector<std::string>>(this);
    connect(authWatcher, &QFutureWatcher<std::vector<std::string>>::finished,
            this, [self, authWatcher]() {
        auto codes = authWatcher->result();
        authWatcher->deleteLater();
        if (!self) return;

        self->ui_->authorityTypeCombo->clear();
        for (const auto& code : codes) {
            self->ui_->authorityTypeCombo->addItem(QString::fromStdString(code));
        }
    });
    authWatcher->setFuture(QtConcurrent::run(authTask));

    // Load subject areas
    auto saTask = [self]() -> std::vector<std::string> {
        if (!self || !self->clientManager_) return {};

        dq::messaging::get_subject_areas_request request;
        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_subject_areas_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {};

        auto response = dq::messaging::get_subject_areas_response::deserialize(*payload_result);
        if (!response) return {};

        std::vector<std::string> names;
        for (const auto& sa : response->subject_areas) {
            names.push_back(sa.name);
        }
        return names;
    };

    auto* saWatcher = new QFutureWatcher<std::vector<std::string>>(this);
    connect(saWatcher, &QFutureWatcher<std::vector<std::string>>::finished,
            this, [self, saWatcher]() {
        auto names = saWatcher->result();
        saWatcher->deleteLater();
        if (!self) return;

        self->ui_->subjectAreaCombo->clear();
        for (const auto& name : names) {
            self->ui_->subjectAreaCombo->addItem(QString::fromStdString(name));
        }
    });
    saWatcher->setFuture(QtConcurrent::run(saTask));

    // Load data domains
    auto ddTask = [self]() -> std::vector<std::string> {
        if (!self || !self->clientManager_) return {};

        dq::messaging::get_data_domains_request request;
        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_data_domains_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {};

        auto response = dq::messaging::get_data_domains_response::deserialize(*payload_result);
        if (!response) return {};

        std::vector<std::string> names;
        for (const auto& dd : response->domains) {
            names.push_back(dd.name);
        }
        return names;
    };

    auto* ddWatcher = new QFutureWatcher<std::vector<std::string>>(this);
    connect(ddWatcher, &QFutureWatcher<std::vector<std::string>>::finished,
            this, [self, ddWatcher]() {
        auto names = ddWatcher->result();
        ddWatcher->deleteLater();
        if (!self) return;

        self->ui_->domainCombo->clear();
        for (const auto& name : names) {
            self->ui_->domainCombo->addItem(QString::fromStdString(name));
        }
    });
    ddWatcher->setFuture(QtConcurrent::run(ddTask));
}

void CodingSchemeDetailDialog::setCreateMode(bool create) {
    isCreateMode_ = create;
    setProvenanceEnabled(!create);
    updateUiState();
}

void CodingSchemeDetailDialog::setScheme(
    const dq::domain::coding_scheme& scheme) {
    scheme_ = scheme;

    ui_->codeEdit->setText(QString::fromStdString(scheme.code));
    ui_->nameEdit->setText(QString::fromStdString(scheme.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(scheme.description));

    if (scheme.uri) {
        ui_->uriEdit->setText(QString::fromStdString(*scheme.uri));
    }

    int authIdx = ui_->authorityTypeCombo->findText(QString::fromStdString(scheme.authority_type));
    if (authIdx >= 0) ui_->authorityTypeCombo->setCurrentIndex(authIdx);

    int saIdx = ui_->subjectAreaCombo->findText(QString::fromStdString(scheme.subject_area_name));
    if (saIdx >= 0) ui_->subjectAreaCombo->setCurrentIndex(saIdx);

    int ddIdx = ui_->domainCombo->findText(QString::fromStdString(scheme.domain_name));
    if (ddIdx >= 0) ui_->domainCombo->setCurrentIndex(ddIdx);

    populateProvenance(scheme.version, scheme.modified_by, scheme.performed_by,
                       scheme.recorded_at, "", scheme.change_commentary);

    updateUiState();
}

void CodingSchemeDetailDialog::setReadOnly(bool readOnly) {
    isReadOnly_ = readOnly;
    updateUiState();
}

void CodingSchemeDetailDialog::updateUiState() {
    ui_->codeEdit->setReadOnly(!isCreateMode_ || isReadOnly_);
    ui_->nameEdit->setReadOnly(isReadOnly_);
    ui_->descriptionEdit->setReadOnly(isReadOnly_);
    ui_->uriEdit->setReadOnly(isReadOnly_);
    ui_->authorityTypeCombo->setEnabled(!isReadOnly_);
    ui_->subjectAreaCombo->setEnabled(!isReadOnly_);
    ui_->domainCombo->setEnabled(!isReadOnly_);

    ui_->saveButton->setVisible(!isReadOnly_);
    ui_->deleteButton->setVisible(!isCreateMode_ && !isReadOnly_);
}

void CodingSchemeDetailDialog::onSaveClicked() {
    QString code = ui_->codeEdit->text().trimmed();
    QString name = ui_->nameEdit->text().trimmed();
    QString description = ui_->descriptionEdit->toPlainText().trimmed();
    QString uri = ui_->uriEdit->text().trimmed();
    QString authorityType = ui_->authorityTypeCombo->currentText();
    QString subjectArea = ui_->subjectAreaCombo->currentText();
    QString domain = ui_->domainCombo->currentText();

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

    dq::domain::coding_scheme scheme;
    scheme.code = code.toStdString();
    scheme.name = name.toStdString();
    scheme.description = description.toStdString();
    scheme.authority_type = authorityType.toStdString();
    scheme.subject_area_name = subjectArea.toStdString();
    scheme.domain_name = domain.toStdString();
    scheme.modified_by = username_;
    scheme.version = isCreateMode_ ? 0 : scheme_.version;

    if (!uri.isEmpty()) {
        scheme.uri = uri.toStdString();
    }

    QPointer<CodingSchemeDetailDialog> self = this;

    struct SaveResult { bool success; std::string message; };

    auto task = [self, scheme]() -> SaveResult {
        if (!self || !self->clientManager_) return {false, "Dialog closed"};

        dq::messaging::save_coding_scheme_request request;
        request.schemes.push_back(scheme);
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_coding_scheme_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = dq::messaging::save_coding_scheme_response::deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this, [self, watcher, code]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            emit self->schemeSaved(code);
            self->notifySaveSuccess(tr("Coding scheme '%1' saved").arg(code));
        } else {
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });

    emit statusMessage(tr("Saving..."));
    watcher->setFuture(QtConcurrent::run(task));
}

void CodingSchemeDetailDialog::onDeleteClicked() {
    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"),
        tr("Delete coding scheme '%1'?").arg(ui_->codeEdit->text()),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<CodingSchemeDetailDialog> self = this;
    QString code = ui_->codeEdit->text();

    auto task = [self, code]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_coding_scheme_request request;
        request.codes.push_back({code.toStdString()});
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_coding_scheme_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_coding_scheme_response::deserialize(*payload_result);
        return response && response->success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher, code]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (success) {
            emit self->statusMessage(tr("Coding scheme deleted successfully"));
            emit self->schemeDeleted(code);
            self->requestClose();
        } else {
            emit self->errorMessage(tr("Failed to delete coding scheme"));
        }
    });

    emit statusMessage(tr("Deleting..."));
    watcher->setFuture(QtConcurrent::run(task));
}

}
