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
#include "ores.qt/SubjectAreaDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_SubjectAreaDetailDialog.h"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

SubjectAreaDetailDialog::SubjectAreaDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::SubjectAreaDetailDialog),
      clientManager_(nullptr),
      isCreateMode_(true),
      isReadOnly_(false) {

    ui_->setupUi(this);
    setupConnections();
}

SubjectAreaDetailDialog::~SubjectAreaDetailDialog() {
    delete ui_;
}

void SubjectAreaDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked,
            this, &SubjectAreaDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked,
            this, &SubjectAreaDetailDialog::onDeleteClicked);
}

void SubjectAreaDetailDialog::setCreateMode(bool create) {
    isCreateMode_ = create;
    updateUiState();
}

void SubjectAreaDetailDialog::setSubjectArea(
    const dq::domain::subject_area& subject_area) {
    subject_area_ = subject_area;

    ui_->nameEdit->setText(QString::fromStdString(subject_area.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(subject_area.description));
    ui_->commentaryEdit->setPlainText(QString::fromStdString(subject_area.change_commentary));

    // Select the domain in combo box
    int index = ui_->domainCombo->findText(QString::fromStdString(subject_area.domain_name));
    if (index >= 0) {
        ui_->domainCombo->setCurrentIndex(index);
    }

    updateUiState();
}

void SubjectAreaDetailDialog::setReadOnly(bool readOnly) {
    isReadOnly_ = readOnly;
    updateUiState();
}

void SubjectAreaDetailDialog::updateUiState() {
    ui_->nameEdit->setReadOnly(!isCreateMode_ || isReadOnly_);
    ui_->domainCombo->setEnabled(isCreateMode_ && !isReadOnly_);
    ui_->descriptionEdit->setReadOnly(isReadOnly_);
    ui_->commentaryEdit->setReadOnly(isReadOnly_);

    ui_->saveButton->setVisible(!isReadOnly_);
    ui_->deleteButton->setVisible(!isCreateMode_ && !isReadOnly_);
}

void SubjectAreaDetailDialog::loadDomains() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load domains: not connected";
        return;
    }

    QPointer<SubjectAreaDetailDialog> self = this;

    struct DomainsResult {
        bool success;
        std::vector<dq::domain::data_domain> domains;
    };

    auto task = [self]() -> DomainsResult {
        if (!self || !self->clientManager_) return {false, {}};

        dq::messaging::get_data_domains_request request;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_data_domains_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, {}};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, {}};

        auto response = dq::messaging::get_data_domains_response::deserialize(*payload_result);
        if (!response) return {false, {}};

        return {true, std::move(response->domains)};
    };

    auto* watcher = new QFutureWatcher<DomainsResult>(this);
    connect(watcher, &QFutureWatcher<DomainsResult>::finished, this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            self->ui_->domainCombo->clear();
            for (const auto& domain : result.domains) {
                self->ui_->domainCombo->addItem(QString::fromStdString(domain.name));
            }
            self->onDomainsLoaded();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Failed to load domains for combo box";
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void SubjectAreaDetailDialog::onDomainsLoaded() {
    // Re-select domain if we have a subject_area set
    if (!subject_area_.domain_name.empty()) {
        int index = ui_->domainCombo->findText(
            QString::fromStdString(subject_area_.domain_name));
        if (index >= 0) {
            ui_->domainCombo->setCurrentIndex(index);
        }
    }
}

void SubjectAreaDetailDialog::onSaveClicked() {
    QString name = ui_->nameEdit->text().trimmed();
    QString domain_name = ui_->domainCombo->currentText();
    QString description = ui_->descriptionEdit->toPlainText().trimmed();
    QString commentary = ui_->commentaryEdit->toPlainText().trimmed();

    if (name.isEmpty()) {
        MessageBoxHelper::warning(this, tr("Validation Error"),
                                  tr("Name is required."));
        return;
    }

    if (domain_name.isEmpty()) {
        MessageBoxHelper::warning(this, tr("Validation Error"),
                                  tr("Domain is required."));
        return;
    }

    dq::domain::subject_area subject_area;
    subject_area.name = name.toStdString();
    subject_area.domain_name = domain_name.toStdString();
    subject_area.description = description.toStdString();
    subject_area.change_commentary = commentary.toStdString();
    subject_area.recorded_by = username_;
    subject_area.version = isCreateMode_ ? 0 : subject_area_.version;

    QPointer<SubjectAreaDetailDialog> self = this;

    struct SaveResult { bool success; std::string message; };

    auto task = [self, subject_area]() -> SaveResult {
        if (!self || !self->clientManager_) return {false, "Dialog closed"};

        dq::messaging::save_subject_area_request request;
        request.subject_area = subject_area;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_subject_area_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = dq::messaging::save_subject_area_response::deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this,
            [self, watcher, name, domain_name]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            emit self->statusMessage(tr("Subject area saved successfully"));
            emit self->subjectAreaSaved(name, domain_name);
            self->requestClose();
        } else {
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });

    emit statusMessage(tr("Saving..."));
    watcher->setFuture(QtConcurrent::run(task));
}

void SubjectAreaDetailDialog::onDeleteClicked() {
    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"),
        tr("Delete subject area '%1'?").arg(ui_->nameEdit->text()),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<SubjectAreaDetailDialog> self = this;
    QString name = ui_->nameEdit->text();
    QString domain_name = ui_->domainCombo->currentText();

    auto task = [self, name, domain_name]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_subject_area_request request;
        dq::messaging::subject_area_key key;
        key.name = name.toStdString();
        key.domain_name = domain_name.toStdString();
        request.keys = {key};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_subject_area_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_subject_area_response::deserialize(*payload_result);
        return response && !response->results.empty() && response->results[0].success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this,
            [self, watcher, name, domain_name]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (success) {
            emit self->statusMessage(tr("Subject area deleted successfully"));
            emit self->subjectAreaDeleted(name, domain_name);
            self->requestClose();
        } else {
            emit self->errorMessage(tr("Failed to delete subject area"));
        }
    });

    emit statusMessage(tr("Deleting..."));
    watcher->setFuture(QtConcurrent::run(task));
}

}
