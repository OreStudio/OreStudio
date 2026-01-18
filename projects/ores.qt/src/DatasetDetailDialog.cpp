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
#include "ores.qt/DatasetDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ui_DatasetDetailDialog.h"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq/messaging/dataset_protocol.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

DatasetDetailDialog::DatasetDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::DatasetDetailDialog),
      clientManager_(nullptr),
      isCreateMode_(true),
      isReadOnly_(false) {

    ui_->setupUi(this);
    ui_->asOfDateEdit->setDate(QDate::currentDate());
    setupConnections();
}

DatasetDetailDialog::~DatasetDetailDialog() {
    delete ui_;
}

void DatasetDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked,
            this, &DatasetDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked,
            this, &DatasetDetailDialog::onDeleteClicked);
}

void DatasetDetailDialog::loadLookupData() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<DatasetDetailDialog> self = this;

    // Load catalogs
    auto catalogTask = [self]() -> std::vector<std::string> {
        if (!self || !self->clientManager_) return {};

        dq::messaging::get_catalogs_request request;
        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_catalogs_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {};

        auto response = dq::messaging::get_catalogs_response::deserialize(*payload_result);
        if (!response) return {};

        std::vector<std::string> names;
        names.push_back(""); // Empty option
        for (const auto& c : response->catalogs) {
            names.push_back(c.name);
        }
        return names;
    };

    auto* catalogWatcher = new QFutureWatcher<std::vector<std::string>>(this);
    connect(catalogWatcher, &QFutureWatcher<std::vector<std::string>>::finished,
            this, [self, catalogWatcher]() {
        auto names = catalogWatcher->result();
        catalogWatcher->deleteLater();
        if (!self) return;

        self->ui_->catalogCombo->clear();
        for (const auto& name : names) {
            self->ui_->catalogCombo->addItem(QString::fromStdString(name));
        }
    });
    catalogWatcher->setFuture(QtConcurrent::run(catalogTask));

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

    // Load origin dimensions
    auto originTask = [self]() -> std::vector<std::string> {
        if (!self || !self->clientManager_) return {};

        dq::messaging::get_origin_dimensions_request request;
        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_origin_dimensions_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {};

        auto response = dq::messaging::get_origin_dimensions_response::deserialize(*payload_result);
        if (!response) return {};

        std::vector<std::string> codes;
        for (const auto& od : response->dimensions) {
            codes.push_back(od.code);
        }
        return codes;
    };

    auto* originWatcher = new QFutureWatcher<std::vector<std::string>>(this);
    connect(originWatcher, &QFutureWatcher<std::vector<std::string>>::finished,
            this, [self, originWatcher]() {
        auto codes = originWatcher->result();
        originWatcher->deleteLater();
        if (!self) return;

        self->ui_->originCombo->clear();
        for (const auto& code : codes) {
            self->ui_->originCombo->addItem(QString::fromStdString(code));
        }
    });
    originWatcher->setFuture(QtConcurrent::run(originTask));

    // Load nature dimensions
    auto natureTask = [self]() -> std::vector<std::string> {
        if (!self || !self->clientManager_) return {};

        dq::messaging::get_nature_dimensions_request request;
        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_nature_dimensions_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {};

        auto response = dq::messaging::get_nature_dimensions_response::deserialize(*payload_result);
        if (!response) return {};

        std::vector<std::string> codes;
        for (const auto& nd : response->dimensions) {
            codes.push_back(nd.code);
        }
        return codes;
    };

    auto* natureWatcher = new QFutureWatcher<std::vector<std::string>>(this);
    connect(natureWatcher, &QFutureWatcher<std::vector<std::string>>::finished,
            this, [self, natureWatcher]() {
        auto codes = natureWatcher->result();
        natureWatcher->deleteLater();
        if (!self) return;

        self->ui_->natureCombo->clear();
        for (const auto& code : codes) {
            self->ui_->natureCombo->addItem(QString::fromStdString(code));
        }
    });
    natureWatcher->setFuture(QtConcurrent::run(natureTask));

    // Load treatment dimensions
    auto treatmentTask = [self]() -> std::vector<std::string> {
        if (!self || !self->clientManager_) return {};

        dq::messaging::get_treatment_dimensions_request request;
        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_treatment_dimensions_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {};

        auto response = dq::messaging::get_treatment_dimensions_response::deserialize(*payload_result);
        if (!response) return {};

        std::vector<std::string> codes;
        for (const auto& td : response->dimensions) {
            codes.push_back(td.code);
        }
        return codes;
    };

    auto* treatmentWatcher = new QFutureWatcher<std::vector<std::string>>(this);
    connect(treatmentWatcher, &QFutureWatcher<std::vector<std::string>>::finished,
            this, [self, treatmentWatcher]() {
        auto codes = treatmentWatcher->result();
        treatmentWatcher->deleteLater();
        if (!self) return;

        self->ui_->treatmentCombo->clear();
        for (const auto& code : codes) {
            self->ui_->treatmentCombo->addItem(QString::fromStdString(code));
        }
    });
    treatmentWatcher->setFuture(QtConcurrent::run(treatmentTask));

    // Load coding schemes
    auto csTask = [self]() -> std::vector<std::string> {
        if (!self || !self->clientManager_) return {};

        dq::messaging::get_coding_schemes_request request;
        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_coding_schemes_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {};

        auto response = dq::messaging::get_coding_schemes_response::deserialize(*payload_result);
        if (!response) return {};

        std::vector<std::string> codes;
        codes.push_back(""); // Empty option
        for (const auto& cs : response->schemes) {
            codes.push_back(cs.code);
        }
        return codes;
    };

    auto* csWatcher = new QFutureWatcher<std::vector<std::string>>(this);
    connect(csWatcher, &QFutureWatcher<std::vector<std::string>>::finished,
            this, [self, csWatcher]() {
        auto codes = csWatcher->result();
        csWatcher->deleteLater();
        if (!self) return;

        self->ui_->codingSchemeCombo->clear();
        for (const auto& code : codes) {
            self->ui_->codingSchemeCombo->addItem(QString::fromStdString(code));
        }
    });
    csWatcher->setFuture(QtConcurrent::run(csTask));

    // Load methodologies
    auto methTask = [self]() -> std::vector<std::pair<boost::uuids::uuid, std::string>> {
        if (!self || !self->clientManager_) return {};

        dq::messaging::get_methodologies_request request;
        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_methodologies_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {};

        auto response = dq::messaging::get_methodologies_response::deserialize(*payload_result);
        if (!response) return {};

        std::vector<std::pair<boost::uuids::uuid, std::string>> methodologies;
        for (const auto& m : response->methodologies) {
            methodologies.push_back({m.id, m.name});
        }
        return methodologies;
    };

    auto* methWatcher = new QFutureWatcher<std::vector<std::pair<boost::uuids::uuid, std::string>>>(this);
    connect(methWatcher, &QFutureWatcher<std::vector<std::pair<boost::uuids::uuid, std::string>>>::finished,
            this, [self, methWatcher]() {
        auto meths = methWatcher->result();
        methWatcher->deleteLater();
        if (!self) return;

        self->methodologies_ = meths;
        self->ui_->methodologyCombo->clear();
        self->ui_->methodologyCombo->addItem(""); // Empty option
        for (const auto& [id, name] : meths) {
            self->ui_->methodologyCombo->addItem(QString::fromStdString(name));
        }
    });
    methWatcher->setFuture(QtConcurrent::run(methTask));
}

void DatasetDetailDialog::setCreateMode(bool create) {
    isCreateMode_ = create;
    updateUiState();
}

void DatasetDetailDialog::setDataset(const dq::domain::dataset& dataset) {
    dataset_ = dataset;

    ui_->nameEdit->setText(QString::fromStdString(dataset.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(dataset.description));
    ui_->commentaryEdit->setPlainText(QString::fromStdString(dataset.change_commentary));
    ui_->sourceSystemEdit->setText(QString::fromStdString(dataset.source_system_id));
    ui_->businessContextEdit->setPlainText(QString::fromStdString(dataset.business_context));
    ui_->lineageDepthSpin->setValue(dataset.lineage_depth);

    if (dataset.catalog_name) {
        int idx = ui_->catalogCombo->findText(QString::fromStdString(*dataset.catalog_name));
        if (idx >= 0) ui_->catalogCombo->setCurrentIndex(idx);
    }

    int saIdx = ui_->subjectAreaCombo->findText(QString::fromStdString(dataset.subject_area_name));
    if (saIdx >= 0) ui_->subjectAreaCombo->setCurrentIndex(saIdx);

    int ddIdx = ui_->domainCombo->findText(QString::fromStdString(dataset.domain_name));
    if (ddIdx >= 0) ui_->domainCombo->setCurrentIndex(ddIdx);

    int originIdx = ui_->originCombo->findText(QString::fromStdString(dataset.origin_code));
    if (originIdx >= 0) ui_->originCombo->setCurrentIndex(originIdx);

    int natureIdx = ui_->natureCombo->findText(QString::fromStdString(dataset.nature_code));
    if (natureIdx >= 0) ui_->natureCombo->setCurrentIndex(natureIdx);

    int treatmentIdx = ui_->treatmentCombo->findText(QString::fromStdString(dataset.treatment_code));
    if (treatmentIdx >= 0) ui_->treatmentCombo->setCurrentIndex(treatmentIdx);

    if (dataset.coding_scheme_code) {
        int csIdx = ui_->codingSchemeCombo->findText(QString::fromStdString(*dataset.coding_scheme_code));
        if (csIdx >= 0) ui_->codingSchemeCombo->setCurrentIndex(csIdx);
    }

    if (dataset.methodology_id) {
        for (size_t i = 0; i < methodologies_.size(); ++i) {
            if (methodologies_[i].first == *dataset.methodology_id) {
                ui_->methodologyCombo->setCurrentIndex(static_cast<int>(i + 1)); // +1 for empty option
                break;
            }
        }
    }

    if (dataset.license_info) {
        ui_->licenseEdit->setText(QString::fromStdString(*dataset.license_info));
    }

    auto asOfDate = std::chrono::duration_cast<std::chrono::seconds>(
        dataset.as_of_date.time_since_epoch()).count();
    ui_->asOfDateEdit->setDate(QDateTime::fromSecsSinceEpoch(asOfDate).date());

    updateUiState();
}

void DatasetDetailDialog::setReadOnly(bool readOnly) {
    isReadOnly_ = readOnly;
    updateUiState();
}

void DatasetDetailDialog::updateUiState() {
    ui_->nameEdit->setReadOnly(isReadOnly_);
    ui_->descriptionEdit->setReadOnly(isReadOnly_);
    ui_->commentaryEdit->setReadOnly(isReadOnly_);
    ui_->sourceSystemEdit->setReadOnly(isReadOnly_);
    ui_->businessContextEdit->setReadOnly(isReadOnly_);
    ui_->licenseEdit->setReadOnly(isReadOnly_);
    ui_->lineageDepthSpin->setEnabled(!isReadOnly_);
    ui_->asOfDateEdit->setEnabled(!isReadOnly_);
    ui_->catalogCombo->setEnabled(!isReadOnly_);
    ui_->subjectAreaCombo->setEnabled(!isReadOnly_);
    ui_->domainCombo->setEnabled(!isReadOnly_);
    ui_->originCombo->setEnabled(!isReadOnly_);
    ui_->natureCombo->setEnabled(!isReadOnly_);
    ui_->treatmentCombo->setEnabled(!isReadOnly_);
    ui_->codingSchemeCombo->setEnabled(!isReadOnly_);
    ui_->methodologyCombo->setEnabled(!isReadOnly_);

    ui_->saveButton->setVisible(!isReadOnly_);
    ui_->deleteButton->setVisible(!isCreateMode_ && !isReadOnly_);
}

void DatasetDetailDialog::onSaveClicked() {
    QString name = ui_->nameEdit->text().trimmed();

    if (name.isEmpty()) {
        MessageBoxHelper::warning(this, tr("Validation Error"),
                                  tr("Name is required."));
        return;
    }

    dq::domain::dataset dataset;
    dataset.id = isCreateMode_ ? boost::uuids::random_generator()() : dataset_.id;
    dataset.name = name.toStdString();
    dataset.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    dataset.change_commentary = ui_->commentaryEdit->toPlainText().trimmed().toStdString();
    dataset.source_system_id = ui_->sourceSystemEdit->text().trimmed().toStdString();
    dataset.business_context = ui_->businessContextEdit->toPlainText().trimmed().toStdString();
    dataset.lineage_depth = ui_->lineageDepthSpin->value();
    dataset.recorded_by = username_;
    dataset.version = isCreateMode_ ? 0 : dataset_.version;

    QString catalogName = ui_->catalogCombo->currentText();
    if (!catalogName.isEmpty()) {
        dataset.catalog_name = catalogName.toStdString();
    }

    dataset.subject_area_name = ui_->subjectAreaCombo->currentText().toStdString();
    dataset.domain_name = ui_->domainCombo->currentText().toStdString();
    dataset.origin_code = ui_->originCombo->currentText().toStdString();
    dataset.nature_code = ui_->natureCombo->currentText().toStdString();
    dataset.treatment_code = ui_->treatmentCombo->currentText().toStdString();

    QString codingScheme = ui_->codingSchemeCombo->currentText();
    if (!codingScheme.isEmpty()) {
        dataset.coding_scheme_code = codingScheme.toStdString();
    }

    int methIdx = ui_->methodologyCombo->currentIndex();
    if (methIdx > 0 && methIdx <= static_cast<int>(methodologies_.size())) {
        dataset.methodology_id = methodologies_[methIdx - 1].first;
    }

    QString license = ui_->licenseEdit->text().trimmed();
    if (!license.isEmpty()) {
        dataset.license_info = license.toStdString();
    }

    QDate asOfDate = ui_->asOfDateEdit->date();
    dataset.as_of_date = std::chrono::system_clock::from_time_t(
        QDateTime(asOfDate, QTime(0, 0)).toSecsSinceEpoch());
    dataset.ingestion_timestamp = std::chrono::system_clock::now();

    QPointer<DatasetDetailDialog> self = this;
    boost::uuids::uuid datasetId = dataset.id;

    struct SaveResult { bool success; std::string message; };

    auto task = [self, dataset]() -> SaveResult {
        if (!self || !self->clientManager_) return {false, "Dialog closed"};

        dq::messaging::save_dataset_request request;
        request.dataset = dataset;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_dataset_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = dq::messaging::save_dataset_response::deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this, [self, watcher, datasetId]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            emit self->datasetSaved(datasetId);
            self->notifySaveSuccess(tr("Dataset saved"));
        } else {
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });

    emit statusMessage(tr("Saving..."));
    watcher->setFuture(QtConcurrent::run(task));
}

void DatasetDetailDialog::onDeleteClicked() {
    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"),
        tr("Delete dataset '%1'?").arg(ui_->nameEdit->text()),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<DatasetDetailDialog> self = this;
    boost::uuids::uuid datasetId = dataset_.id;

    auto task = [self, datasetId]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_dataset_request request;
        request.ids = {datasetId};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_dataset_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_dataset_response::deserialize(*payload_result);
        return response && !response->results.empty() && response->results[0].success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher, datasetId]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (success) {
            emit self->statusMessage(tr("Dataset deleted successfully"));
            emit self->datasetDeleted(datasetId);
            self->requestClose();
        } else {
            emit self->errorMessage(tr("Failed to delete dataset"));
        }
    });

    emit statusMessage(tr("Deleting..."));
    watcher->setFuture(QtConcurrent::run(task));
}

}
