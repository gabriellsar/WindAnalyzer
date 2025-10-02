function createWindParticles() {
        const container = document.querySelector('.wind-particles');
        if (!container) return;
        const particleCount = 15;
        for (let i = 0; i < particleCount; i++) {
            const particle = document.createElement('div');
            particle.className = 'particle';
            particle.style.left = Math.random() * 100 + '%';
            particle.style.animationDelay = Math.random() * 8 + 's';
            particle.style.animationDuration = (8 + Math.random() * 4) + 's';
            container.appendChild(particle);
        }
    }
    document.addEventListener('DOMContentLoaded', createWindParticles);