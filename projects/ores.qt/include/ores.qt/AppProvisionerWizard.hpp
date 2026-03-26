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
#ifndef ORES_QT_APP_PROVISIONER_WIZARD_HPP
#define ORES_QT_APP_PROVISIONER_WIZARD_HPP

#include <boost/uuid/uuid.hpp>
#include <QWizard>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

// Forward declarations — pages are defined in the .cpp.
class AppIdentityPage;
class VersionDetailsPage;
class PlatformsPage;
class PackageUploadPage;
class AuditPage;
class ReviewPage;

/**
 * @brief Step-by-step wizard for provisioning a new compute application.
 *
 * Guides the user through six pages:
 *   1. Application — name and description.
 *   2. Version     — opt-in; wrapper/engine versions and min RAM.
 *   3. Platforms   — dual-list selector (skipped if no version).
 *   4. Package     — upload bundle and confirm URI (skipped if no version).
 *   5. Audit       — change reason and commentary.
 *   6. Review      — summary and final Create action.
 *
 * On completion emits provisioned() so the caller can refresh its models.
 */
class AppProvisionerWizard : public QWizard {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.app_provisioner_wizard";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Page {
        kAppIdentityPage    = 0,
        kVersionDetailsPage = 1,
        kPlatformsPage      = 2,
        kPackageUploadPage  = 3,
        kAuditPage          = 4,
        kReviewPage         = 5,
    };

    explicit AppProvisionerWizard(ClientManager* clientManager,
                                  ChangeReasonCache* changeReasonCache,
                                  const std::string& httpBaseUrl,
                                  QWidget* parent = nullptr);

    /**
     * @brief Called by ReviewPage::validatePage() to perform the server
     *        requests.  Returns true on success (wizard closes), false on
     *        failure (wizard stays open with an error message shown).
     */
    bool submit();

signals:
    void provisioned();

private:
    friend class ReviewPage;  // needs access to page pointers in initializePage()

    ClientManager*     client_manager_;
    ChangeReasonCache* change_reason_cache_;
    std::string        http_base_url_;

    // Pre-generated UUIDs so PackageUploadPage knows the version ID before
    // the version record is saved.
    boost::uuids::uuid app_id_;
    boost::uuids::uuid app_version_id_;

    // Non-owning pointers — pages are owned by QWizard.
    AppIdentityPage*    app_identity_page_;
    VersionDetailsPage* version_details_page_;
    PlatformsPage*      platforms_page_;
    PackageUploadPage*  package_upload_page_;
    AuditPage*          audit_page_;
    ReviewPage*         review_page_;
};

} // namespace ores::qt

#endif
